#lang scribble/doc
@(require (planet cce/scheme:4:1/planet)
          scribble/manual
          (for-label scheme/base
                     scheme/contract))

@title{BulletML}
@author{@(author+email "Jay McCarthy" "jay@plt-scheme.org")}

@defmodule/this-package[]

This package provides an implementation of @link["http://www.asahi-net.or.jp/~cs8k-cyu/bulletml/index_e.html"]{BulletML}. There is a @link["http://www.asahi-net.or.jp/~cs8k-cyu/bulletml/bulletml_applet_e.html"]{Java implementation} and a @link["http://www.asahi-net.or.jp/~cs8k-cyu/bulletml/bulletml_ref_e.html"]{reference manual}.

BulletML is an XML-based markup language for describing the patterns of bullets in @link["http://en.wikipedia.org/wiki/Shoot_'em_up"]{shoot 'em up} video games. These patterns are particularly complicated in @link["http://en.wikipedia.org/wiki/Shoot_'em_up#.22Bullet_hell.22_evolution_and_niche_appeal"]{"bullet hell" or Danmaku} shmups. BulletML is an attempt to make developing these patterns easier.

If you require the package with the require line above, then it will install and launch a demo application where you can select an example and watch it. In the demo program there is a red circle where your mouse cursor is. This will be the target of many bullets, so move it around to see the patterns change. (If the screen is black when it starts, then you've hit a bug in the Universe teachpack; just retry.)