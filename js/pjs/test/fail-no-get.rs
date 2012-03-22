f = fork(function(o) {
    assert(0 == 1, "the world is upside down");
    return 22;
});

oncompletion(function() {
    // we do not call f.get(), error is unobserved
})
