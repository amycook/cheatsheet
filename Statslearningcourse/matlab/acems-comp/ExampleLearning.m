%% SVM learning with 1000 samples
function learnedobj =  ExampleLearning(n, funcprt)
    N = 10^3; % training set
    
    x = double(rand(N,n) <= 0.5);
    y = zeros(N,1);
    for i=1:N
        y(i) =   funcprt(x(i,:));   
    end
    SVMModel = fitensemble(x,y,'AdaBoostM1',2000,'tree');
    learnedobj = SVMModel;
end