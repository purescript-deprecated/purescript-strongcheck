'use strict'

var gulp        = require('gulp')
  , purescript  = require('gulp-purescript')
  , run         = require('gulp-run')
  , jsValidate  = require('gulp-jsvalidate')
  ;

var sources = [
    'src/**/*.purs',
    'bower_components/purescript-*/src/**/*.purs'
];
var foreigns = [
    'src/**/*.js',
    'bower_components/purescript-*/src/**/*.js'
];

var testSources = [
    'test/**/*.purs'
];
var testForeigns = [
    'test/**/*.js'
];

gulp.task('docs', function() {
    return purescript.pscDocs({
        src: sources,
        docgen: {
            "Test.StrongCheck": "docs/Test/StrongCheck.md",
            "Test.StrongCheck.Gen": "docs/Test/StrongCheck/Gen.md",
            "Test.StrongCheck.Landscape": "docs/Test/StrongCheck/Landscape.md",
            "Test.StrongCheck.Perturb": "docs/Test/StrongCheck/Perturb.md"
        }
    });
});

gulp.task('make', function() {
    return purescript.psc({
        src: sources,
        ffi: foreigns
    });
});

gulp.task('test-make', function() {
    return purescript.psc({
        src: sources.concat(testSources),
        ffi: foreigns.concat(testForeigns)
    });
});

gulp.task('test', ['test-make'], function() {
    return purescript.pscBundle({
        src: 'output/**/*.js',
        main: 'Test.Main',
        output: "dist/test.js"
    }).pipe(run("node dist/test.js"));
});

gulp.task('default', ['make', 'docs']);
