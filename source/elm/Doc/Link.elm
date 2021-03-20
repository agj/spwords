module Doc.Link exposing (Link, create, url)


type Link
    = Link
        { url : String
        }


url : Link -> String
url (Link link) =
    link.url


create : String -> Link
create theUrl =
    Link { url = theUrl }
