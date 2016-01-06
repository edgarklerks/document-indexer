Document indexer
===============

A simple way to document all your development related files into one elastic search instance, 
which can be searched through a set of command line utilities. 

Installation
============

Install the haskell platform 

Debian based: 

    apt-get install haskell-platform

Nix based: 

    nix-env -i -A nixpkgs.haskellPlatform

Add ~/.cabal/bin to your path 

    echo 'export PATH="$HOME/.cabal/bin/:$PATH' >> ~/.bash_profile 

or 

    echo 'export PATH="$HOME/.cabal/bin/:$PATH' >> ~/.zshenv 

or 
  
    echo 'export PATH="$HOME/.cabal/bin/:$PATH' >> ~/.shrc


Run the following commands in the build directory:

    cabal configure 
    cabal build
    cabal install


Install elasticsearch*

    nix-env -i -A nixpkgs.elasticsearch

Start the elasticsearch instance (in the source directory, in the non beta version it will be a system service):

    start.sh 

And initialize it:
  
    init_elasticsearch_document_indexer

Usage
=====


Index all your manpages:

-N 8 runs it on 8 cores, it builds them in parallel.

   index_man_pages /usr/share/man /home/eklerks/.nix-profile/share/man /usr/local/share/man/man1  +RTS -N8


Search through the content of the manpages:

  search_man_pages unsigned long \*num_items_return

It will give this back as result:

        XFilterEvent                                                 3                    , **Window** Specifies the **event** to filter. Specifies the **window** for which the filter is to be
        XButtonEvent                                                 3                     request */         Display *display;       /* Display the **event** was read from */         **Window** **window**
        XMotionEvent                                                 3                     request */         Display *display;       /* Display the **event** was read from */         **Window** **window**
        XKeyEvent                                                    3                     request */         Display *display;       /* Display the **event** was read from */         **Window** **window**
        XSelectInput                                                 3                    , unless the do_not_propagate mask prohibits it.  Setting the **event**-mask attribute of a **window** over
        XCrossingEvent                                               3                    ;       /* Display the **event** was read from */         **Window** **window**;  /* ``**event**'' **window** reported
        XDestroySubwindows                                           3                     DestroyNotify **event** for each **window**.  The **window** should never be referenced again.  If the
        XReparentWindow                                              3                     override_redirect member returned in this **event** is set to the window's correspond- ing attribute.  **Window** manager
        XGravityEvent                                                3                     */         **Window** **event**;         **Window** **window**;         int x, y; } XGravityEvent; When you receive this **event**



Index the tags and the content of your projects:

  index_project_dir ~/sources/my-project my-project 

Then search throught the tags:
  
  search_source_files --tags void initialize_window 

or search for a file:

  search_sources_files --files window.py 

or through the content:

  search_source_files --content fuck 

Keep a list of your sources for rebuild
======================================
(Not working yet)

You can create a conf file with all your sources you want to have in your index, so you can easily reupdate them. 

        # Add the nix store path
        [nix]

        nixpath = ["/nix"]

        # add manpages 
        [man]

        man = ["/usr/share/man", "/home/eklerks/.nix-profile/share/man"]

        # Add projects, the label is the project name
        [projects]

        contlib = ["/home/eklerks/sources/sanoma/content-library"]
        home-conf = ["/home/eklerks/sources/vim-zsh-vimperator-xmonad-configuration"]

Errata
======

There is still some stuff not working correctly. I want to search throught the nix store, but I have to decide how to analyze the nix store dir and how to store it, so it is useful.

When things change the index has to be rebuilt. Haven't got a update strategy yet. Path is pretty unique, so I should go for that. 

The manpages titles are not searched. This is not a big issue, because the title is included in the man page. 

When source files are search, all tags in the file are returned, we only want to show the relevant tags.  

More analyze strategies are needed for source files. Comments should be recognized and indexed. Types should be calculated for static languages. Maybe vulnerabilities should be searched and indexed. 

The rebuild commando is not working at the moment. 
