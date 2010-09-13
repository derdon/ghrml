# -*- coding: utf-8 -*-
"""
    ghrml.template
    ~~~~~~~~~~~~~~

    Implements the GHRML template.

    :copyright: Copyright 2008 by Armin Ronacher.
    :license: BSD.
"""
from genshi.template.markup import MarkupTemplate#, Stream
from genshi.core import Namespace
from ghrml.parser import Parser


class GHRMLTemplate(MarkupTemplate):
    """
    Implements the basic GHRML template.
    """

    DEFAULT_NAMESPACE = Namespace('http://www.w3.org/1999/xhtml')
    standalone_directives = {
        'for':          'each',
        'if':           'test',
        'choose':       'test',
        'when':         'test',
        'otherwise':    None,
        'def':          'function',
        'match':        'path'
    }

    def __init__(self, source, basedir=None, filename=None, loader=None,
                 encoding=None, lookup='strict', allow_exec=True,
                 default_namespace=None):
        if default_namespace is None:
            default_namespace = self.DEFAULT_NAMESPACE
        self.default_namespace = default_namespace
        MarkupTemplate.__init__(self, source, basedir=basedir,
                                filename=filename, loader=loader,
                                encoding=encoding, lookup=lookup,
                                allow_exec=allow_exec)

    def _parse(self, source, encoding):
        if isinstance(source, str):
            source = source.decode(encoding)
        parser = Parser(source, self.filename, lookup=self.lookup,
                        inline_directives=dict(self.directives),
                        standalone_directives=self.standalone_directives)
        parser.parse()
        return MarkupTemplate._parse(self, parser.stream, encoding)
