# -*- coding: utf-8 -*-
"""
    ghrml.parser
    ~~~~~~~~~~~~

    Implements a GHRML parser that returns a simple syntax tree of
    tuples and lists.

    :copyright: Copyright 2008 by Armin Ronacher.
    :license: BSD.
"""
import re
import tokenize
from compiler import ast
from itertools import chain
from genshi.core import START, END, XML_DECL, DOCTYPE, TEXT, START_NS, \
                        END_NS, START_CDATA, END_CDATA, PI, COMMENT, \
                        Attrs, QName, Stream
from genshi.template.base import EXPR
from genshi.template.eval import Expression, _parse


_tag_re = re.compile('%([a-zA-Z_][a-zA-Z0-9_]*(?::[a-zA-Z_][a-zA-Z0-9_]*)?)')
_class_re = re.compile(r'\.([a-zA-Z_][a-zA-Z0-9_]*)')
_ident_re = re.compile(r'#([a-zA-Z_][a-zA-Z0-9_]*)')
_attr_start_re = re.compile(r'\{\s*')
_directive_start_re = re.compile(r'\[\s*([a-zA-Z_][a-zA-Z0-9_]*)\s+')
_inline_start_re = re.compile(r'%\(')
_token_re = re.compile(r'%s|%s' % (r'[uU]?[rR]?("""|\'\'\')',
                                   tokenize.PseudoToken))
_triple_ends = (tokenize.single3prog, tokenize.double3prog)


INSTRUCTIONS = '.%#'
DEFAULT_XMLNS = {
    '':     'http://www.w3.org/1999/xhtml',
    'py':   'http://genshi.edgewall.org/',
    'xi':   'http://www.w3.org/2001/XInclude'
}
DEFAULT_TAG = QName(u'http://www.w3.org/1999/xhtml}div')


def _coalesce(stream):
    """Coalesces adjacent TEXT events into a single event."""
    textbuf = []
    textpos = None
    for kind, data, pos in chain(stream, [(None, None, None)]):
        if kind is TEXT:
            textbuf.append(data)
            if textpos is None:
                textpos = pos
        else:
            if textbuf:
                value = u''.join(textbuf)
                textbuf = []
                if value.endswith('\n'):
                    value = value[:-1]
                yield TEXT, value, textpos
                textpos = None
            if kind:
                yield kind, data, pos


class ParserError(Exception):
    """
    Generic parser error.  This error never reaches the user because the
    template will translate it into a genshi `TemplateSyntaxError`.
    """

    def __init__(self, msg, filename=None, lineno=None, offset=None):
        Exception.__init__(self, msg)
        self.filename = filename
        self.lineno = lineno
        self.offset = offset


class IndentationError(ParserError):
    """
    Raised if the `LineTokenizer` is unable to process an indentation.
    """


class LineTokenizer(object):
    """
    Tokenize the given string or iterable (has to yield lines either with, or
    without the trailing newline) and resolve the indention according to the
    rules of the GHRML syntax.  The tokenizer implements the iterator protocol
    and yields tuples in the form ``(lineno, offset, level, line)``.

    If the current indentation is greater than the indentation of the
    outer block:

    1.  If the line starts with a processing character (``%``, ``.``, or
        ``#``) the current indentation is added to an internal indentaition
        stack, flagged as "processing level".

    2.  If the tokenizer hasn't processed the line yet, it checks if the
        top of the stack is flagged as "processing level".  In that case it
        will add a new "data level" to the stack and handle the indentation.

    3.  Now the only thing that could have happenend is that the current
        level on the stack is flagged as "data level" which means that we
        use the raw line (with the full indentation) and strip of the
        indentation up to the current level.

    If the tokenizer has problems undenting (it will not complain on missing
    expected indents because the syntax allows empty bodes for each level) an
    `IndentationError` is raised.
    """

    def __init__(self, source, filename=None):
        if isinstance(source, basestring):
            source = unicode(source).splitlines()
        self.lineiter = iter(source)
        self.filename = filename
        self.lineno = 0
        self._levels = [(0, True)]

    def __iter__(self):
        return self

    def next(self):
        line = self.lineiter.next()
        self.lineno += 1
        raw_line = line.rstrip().expandtabs()
        line = raw_line.lstrip()
        indentation = len(raw_line) - len(line)

        if line:
            if indentation > self._levels[-1][0]:
                if line[0] in INSTRUCTIONS:
                    self._levels.append((indentation, True))
                elif self._levels[-1][1]:
                    self._levels.append((indentation, False))
                else:
                    line = raw_line[self._levels[-1][0]:]
            elif indentation < self._levels[-1][0]:
                for level, _ in self._levels:
                    if level == indentation:
                        while self._levels[-1][0] != level:
                            self._levels.pop()
                        break
                else:
                    raise IndentationError('invalid dedent', self.filename,
                                           lineno, indentation)
            if line[0] == '\\':
                line = line[1:]

        return self.lineno, indentation, len(self._levels) - 1, line


class Parser(object):
    """
    A generator based GHRML parser.
    """

    def __init__(self, source, filename=None, default_tag=DEFAULT_TAG,
                 basedir=None, lookup='strict', inline_directives=(),
                 standalone_directives=None):
        self.filename = filename
        self.lineno = self.indentation = self.offset = 0
        self.line = None
        self.basedir = basedir
        self.lookup = lookup
        self.inline_directives = inline_directives
        self.standalone_directives = standalone_directives or {}
        self._line_tokenizer = LineTokenizer(source, filename)
        self._xmlns_stack = {}
        self._queue = {}
        self.parsed = False
        self.result = []
        self.add = self.result.append

    def __iter__(self):
        if not self.parsed:
            self.parse()
        return iter(self.stream)

    @property
    def stream(self):
        """The stream for the results."""
        if self.parsed:
            return Stream(self.result).filter(_coalesce)

    def add(self, event):
        """Add a new event to the result buffer."""

    def get_namespace(self, name):
        """
        Return the namespace for a name for the current indentation.

        :return: namespace uri or `None`
        """
        for x in xrange(self.indentation, -1, -1):
            if x in self._xmlns_stack:
                rv = self._xmlns_stack[x].get(name)
                if rv is not None:
                    return rv

    def have_namespace(self, name):
        """
        Check if a namespace is known to the parser.

        :return: `True` of `False`.
        """
        return self.get_namespace(name) is not None

    def set_namespace(self, name, uri):
        """
        Set the namespace of a name for the current indentation.  The return
        value is a markup event generator that emits the corresponding
        `START_NS` event.
        """
        self._xmlns_stack.setdefault(self.indentation, {})[name] = uri
        self.add((START_NS, (name, uri), self.make_pos(0)))

    def get_qname(self, name):
        """
        Return a `QName` for a name.

        :return: qname
        """
        if ':' in name:
            ns, localname = name.split(':')
            name = self.get_namespace(ns)
            if name is None:
                self.fail('The namespace alias %s is unknown' % ns)
        else:
            localname = name
            name = self.get_namespace('')
        return QName(u'%s}%s' % (name, localname))

    def make_pos(self, offset, lineno=None):
        """Return a position tuple."""
        if lineno is None:
            lineno = self.lineno
        return (self.filename, lineno, self.offset + offset)

    def enqueue(self, event):
        """
        Enqueue an event for the current indentation level so that it's properly
        closed when the stack is left.
        """
        self._queue.setdefault(self.indentation, []).append(event)

    def fail(self, msg='syntax error', offset=0, cls=ParserError):
        """Fail with an error."""
        raise cls(msg, *self.make_pos(offset))

    def read_line(self, toplevel=True):
        old_indentation = self.indentation
        try:
            self.lineno, self.offset, self.indentation, self.line = \
                self._line_tokenizer.next()
        except StopIteration:
            if not toplevel:
                self.fail('unexpected end of block')
            self.line = None
            self.indentation = self.offset = 0
        if old_indentation >= self.indentation:
            if not toplevel:
                self.fail('invalid dedent', cls=IndentationError)
            for x in xrange(old_indentation, self.indentation - 1, -1):
                events = self._queue.pop(x, ())
                while events:
                    self.add(events.pop())
                namespaces = self._xmlns_stack.pop(x, ())
                for name in namespaces:
                    self.add((END_NS, name, self.make_pos(0)))
        return self.line

    def parse(self):
        """
        Start the parsing.  Results end up in `results`.

        :raises ParseError: if the GHRML source is not well formed.
        :raises RuntimeError: if the source is already parsed
        """
        if self.parsed:
            raise RuntimeError('source already parsed')
        self.read_line()
        if not self._xmlns_stack:
            self._xmlns_stack[0] = DEFAULT_XMLNS.copy()
        while self.line is not None:
            self.parse_line()
            self.read_line()
        self.parsed = True

    def parse_line(self, offset=0):
        """
        Parse the current line.

        :raises ParseError: if the GHRML source is not well formed.
        """
        line = self.line[offset:]
        if not line or line[:2] == '//':
            return
        elif line[0] == '-':
            self.parse_code_block(offset)
        elif line[0] == '[':
            self.parse_standalone_instruction(offset)
        elif line[0] in INSTRUCTIONS:
            self.parse_instruction(offset)
        else:
            self.add((TEXT, line + '\n', self.make_pos(offset)))

    def parse_code_block(self, offset=0):
        """
        Parse a code block.

        :raises ParserError: if it encounters invalid syntax.
        """
        pos = self.make_pos(offset)
        code_buffer = []
        block_indention = self.indentation
        line = self.line[offset:]
        while line[:1] == '-' and self.indentation == block_indention:
            code_buffer.append(line[1:])
            line = self.read_line()
            offset = self.offset
        self.add((PI, ('python', '\n'.join(code_buffer)), pos))

    def parse_standalone_instruction(self, offset=0):
        directive_pos = self.make_pos(offset)
        line = self.line
        match = _directive_start_re.match(line, offset)
        if match is None:
            self.fail('directive name expected', offset)
        directive_name = match.group(1)
        if directive_name not in self.standalone_directives:
            if directive_name in self.inline_directives:
                self.fail('directive %r has no standalone mode' %
                          directive_name, offset)
            self.fail('unknown directive %r' % directive_name, offset)

        pos = match.end()
        level = 1
        directive_buffer = []
        while level:
            match = _token_re.match(line, pos)
            if match is None:
                self.fail(offset=pos)
            total, string = match.group(0, 1)
            directive_buffer.append(total)
            # tripple quoted strings are special, because they can
            # span multiple lines.
            if string is not None:
                endprog = _triple_ends[string == '"""']
                while 1:
                    match = endprog.search(line, pos)
                    if match is not None:
                        pos = match.end()
                        break
                    line = self.read_line(False)
                    pos = 0
            # we have a regular match here, just count the braces.
            else:
                total = total.strip()
                if total == '[':
                    level += 1
                elif total == ']':
                    level -= 1
                pos = match.end()

        # look up directive attribute and create tag and attr
        end_pos = len(line)
        attr_name = self.standalone_directives[directive_name]
        attr_value = ' '.join(directive_buffer[:-1]).strip()
        if attr_name is None and attr_value:
            self.fail('directive %r accepts no arguments' %
                      directive_name)
        attr = Attrs(((QName(attr_name), attr_value),))
        tag = QName('http://genshi.edgewall.org/}' + directive_name)

        # yield the start tag
        self.add((START, (tag, attr), directive_pos))

        # enqueue the tag so that it's closed for us when the stack is left.
        self.enqueue((END, tag, self.make_pos(pos)))

        # is there data left on the line?  go back to the line parser!
        if pos < end_pos:
            self.parse_line(pos)

    def parse_instruction(self, offset=0):
        """
        Parse the instruction embedded in the line.
        """
        start_lineno = self.lineno
        line = self.line

        # parse the start tag
        match = _tag_re.match(line, offset)
        if match is not None:
            tag = self.get_qname(match.group(1))
            pos = match.end()
        else:
            tag = self.default_tag
            pos = 0

        # parse element attributes
        xmlns = {}
        attrs = []
        classes = []
        ident = None
        while 1:
            # class names with CSS class name syntax
            match = _class_re.match(line, pos)
            if match is not None:
                classes.append(match.group(1))
                pos = match.end()
                continue

            # identifiers with CSS identifier syntax
            match = _ident_re.match(line, pos)
            if match is not None:
                if ident is not None:
                    self.fail('Element can\'t have multiple IDs', pos)
                ident = match.group(1)
                pos = match.end()
                continue

            # attributes definitions
            match = _attr_start_re.match(line, pos)
            if match is not None:
                pos = match.end()
                level = 1
                attr_buffer = []
                while level:
                    match = _token_re.match(line, pos)
                    if match is None:
                        self.fail(offset=pos)
                    total, string = match.group(0, 1)
                    attr_buffer.append(total)
                    # tripple quoted strings are special, because they can
                    # span multiple lines.
                    if string is not None:
                        endprog = _triple_ends[string == '"""']
                        while 1:
                            match = endprog.search(line, pos)
                            if match is not None:
                                pos = match.end()
                                break
                            line = self.read_line(False)
                            pos = self.offset
                    # we have a regular match here, just count the braces.
                    else:
                        total = total.strip()
                        if total == '{':
                            level += 1
                        elif total == '}':
                            level -= 1
                        pos = match.end()
                attrs.append((QName('http://genshi.edgewall.org/}attrs'),
                              '{%s}' % ' '.join(attr_buffer[:-1])))
                continue

            # directives
            match = _directive_start_re.match(line, pos)
            if match is not None:
                directive_name = match.group(1)
                if directive_name not in self.inline_directives:
                    self.fail('unknown directive %r' % directive_name, pos)
                pos = match.end()
                level = 1
                directive_buffer = []
                while level:
                    match = _token_re.match(line, pos)
                    if match is None:
                        self.fail(offset=pos)
                    total, string = match.group(0, 1)
                    directive_buffer.append(total)
                    # tripple quoted strings are special, because they can
                    # span multiple lines.
                    if string is not None:
                        endprog = _triple_ends[string == '"""']
                        while 1:
                            match = endprog.search(line, pos)
                            if match is not None:
                                pos = match.end()
                                break
                            line = self.read_line(False)
                            pos = self.offset
                    # we have a regular match here, just count the braces.
                    else:
                        total = total.strip()
                        if total == '[':
                            level += 1
                        elif total == ']':
                            level -= 1
                        pos = match.end()
                attrs.append((QName('http://genshi.edgewall.org/}' +
                                    directive_name),
                              ' '.join(directive_buffer[:-1])))
                continue

            end_pos = len(line)
            pos = end_pos - len(line[pos:].lstrip())
            break

        # add special attributes class and id and set the namespaces.
        if classes:
            attrs.append((self.get_qname('class'), u' '.join(classes)))
        if ident is not None:
            attrs.append((self.get_qname('id'), ident))
        for name, uri in xmlns.iteritems():
            self.set_namespace(name, uri)

        # yield the start tag
        self.add((START, (tag, Attrs(attrs)), self.make_pos(0, start_lineno)))

        # enqueue the tag so that it's closed for us when the stack is left.
        self.enqueue((END, tag, self.make_pos(end_pos)))

        # is there data left on the line?  go back to the line parser!
        if pos < end_pos:
            self.parse_line(pos)
