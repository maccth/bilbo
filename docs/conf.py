# Configuration file for the Sphinx documentation builder.
#
# This file only contains a selection of the most common options. For a full
# list see the documentation:
# http://www.sphinx-doc.org/en/master/config

# -- Path setup --------------------------------------------------------------

# If extensions (or modules to document with autodoc) are in another directory,
# add these directories to sys.path here. If the directory is relative to the
# documentation root, use os.path.abspath to make it absolute, like shown here.
#
# import os
# import sys
# sys.path.insert(0, os.path.abspath('.'))


# -- Project information -----------------------------------------------------

project = 'Bilbo'
copyright = '2019, Christopher Macca'
author = 'Christopher Macca'


# -- General configuration ---------------------------------------------------

# Add any Sphinx extension module names here, as strings. They can be
# extensions coming with Sphinx (named 'sphinx.ext.*') or your custom
# ones.
extensions = [
]

# Add any paths that contain templates here, relative to this directory.
templates_path = ['_templates']

# List of patterns, relative to source directory, that match files and
# directories to ignore when looking for source files.
# This pattern also affects html_static_path and html_extra_path.
exclude_patterns = ['_build', 'Thumbs.db', '.DS_Store']


# -- Options for HTML output -------------------------------------------------

# The theme to use for HTML and HTML Help pages.  See the documentation for
# a list of builtin themes.
#

# alabaster
html_theme = 'classic'

# Add any paths that contain custom static files (such as style sheets) here,
# relative to this directory. They are copied after the builtin static files,
# so a file named "default.css" will overwrite the builtin "default.css".
html_static_path = ['_static']

html_theme_options = {
    "collapsiblesidebar" : True,
#     "footerbgcolor"     : "#e7e9f7",
#     "footertextcolor"   : "#252e6a",
    "sidebarbgcolor"    : "#fff",
# #     "sidebarbtncolor"   :
    "sidebartextcolor"  : "#424747",
    "sidebarlinkcolor"  : "#5392ff",
    "relbarbgcolor"     : "#25467a",
#     "relbartextcolor"   : "#252e6a",
# #     "relbarlinkcolor"   :
    "bgcolor"   : "#fff",
#     "textcolor"         : "#252e6a",
# #     "linkcolor"         :
# #     "visitedlinkcolor"  :
    # "headbgcolor"       : "#25467a",
#     "headtextcolor"     : "#252e6a",
#     "headlinkcolor"     : "#252e6a",
# #     "codebgcolor"       :
# #     "codetextcolor"     :
# #     "bodyfont"          :
#     "headfont"  : "Helvetica",
}

# footerbgcolor (CSS color): Background color for the footer line.
# footertextcolor (CSS color): Text color for the footer line.
# sidebarbgcolor (CSS color): Background color for the sidebar.
# sidebarbtncolor (CSS color): Background color for the sidebar collapse button (used when collapsiblesidebar is True).
# sidebartextcolor (CSS color): Text color for the sidebar.
# sidebarlinkcolor (CSS color): Link color for the sidebar.
# relbarbgcolor (CSS color): Background color for the relation bar.
# relbartextcolor (CSS color): Text color for the relation bar.
# relbarlinkcolor (CSS color): Link color for the relation bar.
# bgcolor (CSS color): Body background color.
# textcolor (CSS color): Body text color.
# linkcolor (CSS color): Body link color.
# visitedlinkcolor (CSS color): Body color for visited links.
# headbgcolor (CSS color): Background color for headings.
# headtextcolor (CSS color): Text color for headings.
# headlinkcolor (CSS color): Link color for headings.
# codebgcolor (CSS color): Background color for code blocks.
# codetextcolor (CSS color): Default text color for code blocks, if not set differently by the highlighting style.
# bodyfont (CSS font-family): Font for normal text.
# headfont (CSS font-family): Font for headings.
