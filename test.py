from ghrml import GHRMLTemplate


print GHRMLTemplate("""

- foo = 42

%html
  [def bar()] 42

  %head
    %title $title - $foo

  %body
    %p{'id': 'foo'}
      Hello World
    #bar
      dum dum
    %ul
      %li[for item in seq] $item
    ${bar()}

""").generate(title="Page Title", seq=range(10)).render("html", doctype='html')
