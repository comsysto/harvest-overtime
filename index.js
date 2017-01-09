require('tachyons')
const Elm = require('./Main.elm')
const config = require('config')

Elm.Main.embed(document.getElementsByTagName('body')[0], config)
