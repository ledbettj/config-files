var left = '0', right = '1';

slate.config('defaultToCurrentScreen', false);

var leftFull = function(monitor) {
  return slate.operation('move', {
    screen: monitor,
    x:      'screenOriginX',
    y:      'screenOriginY',
    width:  'screenSizeX/2',
    height: 'screenSizeY'
  });
};

var rightFull = function(monitor) {
  return slate.operation('move', {
    screen: monitor,
    x:      'screenOriginX+screenSizeX/2',
    y:      'screenOriginY',
    width:  'screenSizeX/2',
    height: 'screenSizeY'
  });
};

var topFull = function(monitor) {
  return slate.operation('move', {
    screen: monitor,
    x:      'screenOriginX',
    y:      'screenOriginY',
    width:  'screenSizeX',
    height: 'screenSizeY/2'
  });
};

var bottomFull = function(monitor) {
  return slate.operation('move', {
    screen: monitor,
    x:      'screenOriginX',
    y:      'screenOriginY+screenSizeY/2',
    width:  'screenSizeX',
    height: 'screenSizeY/2'
  });
};

var topRight = function(monitor) {
  return slate.operation('move', {
    screen: monitor,
    x:      'screenOriginX+screenSizeX/2',
    y:      'screenOriginY',
    width:  'screenSizeX/2',
    height: 'screenSizeY/2'
  });
};

var topLeft = function(monitor) {
  return slate.operation('move', {
    screen: monitor,
    x:      'screenOriginX',
    y:      'screenOriginY',
    width:  'screenSizeX/2',
    height: 'screenSizeY/2'
  });
};

var bottomRight = function(monitor) {
  return slate.operation('move',  {
    screen: monitor,
    x:      'screenOriginX+screenSizeX/2',
    y:      'screenOriginY+screenSizeY/2',
    width:  'screenSizeX/2',
    height: 'screenSizeY/2'
  });
};

var bottomLeft = function(monitor) {
  return slate.operation('move',  {
    screen: monitor,
    x:      'screenOriginX',
    y:      'screenOriginY+screenSizeY/2',
    width:  'screenSizeX/2',
    height: 'screenSizeY/2'
  });
};

slate.layout('worksies', {
  iTerm:           { operations: [bottomRight(right)] },
  'Google Chrome': { operations: [topRight(right)]    },
  Emacs:           { operations: [leftFull(right)]    },
  Komanda:         { operations: [rightFull(left)]    },
  HipChat:         { operations: [topLeft(left)]      },
  Mail:            { operations: [bottomLeft(left)]   }
});

slate.bind('1:cmd', slate.operation('layout', {name: 'worksies'}));
slate.bind('g:cmd', slate.operation('grid',  {
  grids: {
    '1440x900': {
      width:  4,
      height: 4
    },

    '2560x1440': {
      width:  6,
      height: 6
    },
  },
  padding: 5
}));
