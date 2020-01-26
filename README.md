# git-comtemplate

Small utility that prepares commit templates with a story id and co-authors.

Running any command with `-h` or without any arguments shows a help page, except
`git comtemplate reset`, which will disable commit templates until another
command is run again.

## Who is this project for?

Let's say you or your team want git commit messages that adhere to a consistent
format. Typing it manually on each commit is error-prone and distracts you from
the actual commit message.

Enter git-comtemplate. When you start working on a story, use
`git comtemplate story NICE-1` to set the story id and `git comtemplate authors
fh jd` and get coding. When you call `git commit` later on, your editor will be
pre-filled with the following message:

```
NICE-1: 

Co-authored-by: Finn the Human <finn@thetreehouse.com>
Co-authored-by: Jake the Dog <jake@also.thetreehouse.com>
```

There is even a space after the colon: If you're using vim, you can just press
A end start typing.

*Slightest of Warnings*: `git-comtemplate` will overwrite your user
`commit.template` setting. If you are using this for something else, this
project might not work for you. Also, it will probably interact weirdly with
other git commit templating tools.

## Author initials

The mapping of initials that you can specify to `git comtemplate authors` has to
be maintained by hand. They are stored in `git-comtemplate/authors.dhall` in
your user config directory
([XDG_CONFIG_HOME](https://specifications.freedesktop.org/basedir-spec/basedir-spec-0.6.html),
should default to ~/.config/).
If that sounds confusing, don't worry: Running `git comtemplate` will show you
the full file names.

The file is a [dhall config file](https://dhall-lang.org/), but don't worry, if
you saw json or wrote some code before it should look familiar.

To get you started, `git comtemplate exampleAuthorsFile` will create the file
with some example data if it doesn't exist.

## Building/Installing

git-comtemplate is written in
[Haskell](https://qph.fs.quoracdn.net/main-qimg-086fb2e3079bd6fc4045d4da907fa4f5.webp).
If you want to learn about Haskell, [Learn You a Haskell for Great Good!
](http://learnyouahaskell.com/) is a nice introduction and can be read online
for free.

This project uses [stack](https://haskellstack.org) as a build tool. Stack takes
care of downloading the compiler and all that stuff for us. It should
be available in major package repositories. Here are a few examples:

<dl>
  <dt>MacOS (with [homebrew](https://brew.sh/))</dt>
  <dd>`brew install haskell-stack`</dd>
  <dt>Nix</dt>
  <dd>`nix-env -i stack`</dd>
  <dt>Arch Linux</dt>
  <dd>`pacman -S stack`</dd>
</dl>

When you have stack installed, open a shell in the project folder and execute
`stack build`. It takes a while on the first run, but that should be it!

Stack puts the binary in a bit of a weird place. The following command prints
the full path to the binary:

`echo "$PWD"/$(stack path --dist-dir)/build/git-comtemplate/git-comtemplate`

Put it anywhere on your PATH and you will be able to call it as either
`git-comtemplate` or `git comtemplate`

### Common Issues
When I tried building it on debian 10, I got the following error:

```
<lots of output>
 (ConnectionFailure Network.BSD.getProtocolByName: does not exist (no such protocol name: tcp))
```

This can be fixed by installing netbase: `apt install netbase`

## FAQ

<dl>
    <dt>I changed my mind, how can I get rid of everything this did?</dt>
    <dd>Run <code>git comtemplate</code>. In the text, it mentions two directories, one for
    config (probably <code>~/.config/git comtemplate</code>) and one for it's state
    (probably <code>~/.local/share/git comtemplate</code>). Run <code>git comtemplate reset</code> (or
    unset the <code>commit.template</code> git setting yourself) and delete the two
    <code>git comtemplate</code> folders and nothing will remain.</dd>
    <dt>This is way too much typing. Why are the names so long?</dt>
    <dd>You can use `git comtemplate s` instead of `... story` as well as `a`
    for `authors`. If you think `comtemplate` is too long, you can run `git
    config --global alias.ctp comtemplate` and use `git ctp s STORY-42`.
    <br>
    Given that you will probably use this in a team, I suggest you agree on an
    alias to facilitate pairing. My proposal is <code>git ctp</code>, which can be set up
    with the above commands.</dd>
    <dt>This is great, but I want to use a different template</dt>
    <dd>I am sure you have a valid reason, but I would rather not add the
    complexity of supporting different templates. The problem isn't so much the
    templating itself but the configuration around it.
    <br>
    Right now, <code>git-comtemplate</code> has very narrowly defined code paths, and
    I would prefer to keep it this way. Maybe you can fork this project? If you
    are ready to put a few minutes in, open an issue and we'll talk about it.
    </dd>
</dl>


## Licensing

This application is licensed under the GNU General Public License version 3 or
later.
