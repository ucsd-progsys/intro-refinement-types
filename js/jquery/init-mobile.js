   $(document).bind('mobileinit',function(){
        $.mobile.changePage.defaults.changeHash = false;
        $.mobile.hashListeningEnabled = false;
        $.mobile.pushStateEnabled = false;
    });
