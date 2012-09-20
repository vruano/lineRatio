

lineRatio_processInput = function(x,input.name="") {
  if (is.null(dim(x))) dim(x) = c(length(x),1);
  if (ncol(x) == 1) x = cbind(1:length(x),x);
  x;
}

lineRatio_interpolate = function(x,idx,i) {
  if (x[idx,1] == i) return(x[idx,2]);
  low = idx;
  high = idx;
  if (x[idx,1] < i) {
    if (idx >= length(x[,1])) low = idx - 1 else high = idx + 1;
  }
  else {
    if (idx == 1) high = 2 else low = idx - 1;
  }
  if (low < 1 | high > length(x[,1])) return(x[idx,2]);
  low.val = x[low,2];
  high.val = x[high,2];
  i.diff = x[high,1] - x[low,1];
  slope = (high.val - low.val) / i.diff;
  return (low.val + slope * (i - x[low,1]));
}; 

lineRatio.default = function(x,y,at=NULL,points=NULL) {
  x = lineRatio_processInput(x);
  y = lineRatio_processInput(y);
  if (is.null(points) & is.null(at)) { 
    points=ceiling(max(10,sqrt(length(x[,2]))));
    range = c(min(x[,2],y[,2]),max(x[,2],y[,2]));
    at = seq(range[1],range[2],(range[2] - range[1]) / (max(1,points - 1)));
  } else {
    if (is.null(points)) {
      points = length(at);
    } 
    if (is.null(at)) {
      range = c(min(x[,2],y[,2]),max(x[,2],y[,2]));
      at = seq(range[1],range[2],(range[2] - range[1]) / (max(1,points - 1)));
    }
  } 

  res = sapply(at,simplify=T,function(i) {
    x.idx = length(x[,1]);
    y.idx = length(y[,1]);
    for (n in 1:length(x[,1])) if (x[n,1] >= i) { x.idx = n; break };
    for (n in 1:length(y[,1])) if (y[n,1] >= i) { y.idx = n; break };
    x.val = lineRatio_interpolate(x,x.idx,i);
    y.val = lineRatio_interpolate(y,y.idx,i);
    x.val / y.val;
  });
  
  return(cbind(at,res));
};


lineRatio.density = function(x,y,...) {
  lineRatio.default(cbind(x$x,x$y),cbind(y$x,y$y),...);
}

lineRatio = function(x,y,at,points) UseMethod("lineRatio",x);

