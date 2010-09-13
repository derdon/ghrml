# -*- coding: utf-8 -*-
"""
    ghrml
    ~~~~~

    Genshi Human Readable Markup Language.  Based on Ruby's HAML and powered
    by the Genshi template engine, GHRML acts as alternative syntax for
    Genshi.  It has a slightly different syntax than HAML because after all,
    it's Python and not Ruby, but the core ideas are the same.

    This piece of Genshi Code::

        <html xmlns="http://www.w3.org/1999/xhtml"
              xmlns:py="http://genshi.edgewall.org/"
              lang="en">
          <!-- !This is a comment that will not appear in the output -->
          <head>
            <title>$title</title>
            <script type="text/javascript" src="foo.js"/>
          </head>
          <!-- this comment will appear in the output -->
          <body>
            <div id='content'>
              <div class='left column'>
                <h2>Welcome to our site!</h2>
                <p>${print_information()}</p>
              </div>
              <div class="right column">
                <ul>
                  <li py:for="item in seq">$item</li>
                </ul>
              </div>
            </div>
          </body>
        </html>

    Looks like this in XAML::

        %html{lang='en'}
          ## This is a comment that will not appear in the output
          %head
            %title= title
            %script{type='text/javascript', src='foo.js'}/
          / This comment will appear in the output
          %body
            #content
              .left.column
                %h2 Welcome to our site!
                %p= print_information()
              .right.column
                %ul[for item in seq]
                  %li= item

    Per default the XML namespace for XAML templates will be
    "http://www.w3.org/1999/xhtml".  You can override this via ordinary xmlns
    attributes of the root tag.  The second namespace available per default is
    py: which will of course point to "http://genshi.edgewall.org/".

    :copyright: Copyright 2008 by Armin Ronacher.
    :license: BSD.
"""
from ghrml.template import GHRMLTemplate
