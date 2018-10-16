function [ fh, axh, aucs, opt_threshs ] = plot_roc( ...
    classes, ...
    labels, ...
    scores, ...
    colors ...
    )

[ fh, axh ] = set_up_figure();
plot_equality_line( axh );

%% Plot Classes
if numel( classes ) > 2 % multiple classes
    aucs = cell( length( classes ), 1 );
    opt_threshs = cell( length( classes ), 1 );
    for i = 1 : length( classes )
        [ ph, roc_x, roc_y, thresholds, aucs{ i }, opts ] = plot_roc_for_class( ...
            axh, ...
            labels, ...
            scores( :, i ), ...
            classes( i ), ...
            colors{ mod( i - 1, numel( colors ) ) + 1 } ...
            );
        opt_threshs{ i } = determine_optimal_threshold( roc_x, roc_y, thresholds, opts );
    end
    plot_legend( axh, classes );
else % one class
    positive_class_index = 2;
    aucs = cell( 1, 1 );
    opts = cell( 1, 1 );
    [ ph, roc_x, roc_y, thresholds, aucs{ 1 }, opts ] = plot_roc_for_class( ...
        axh, ...
        labels, ...
        scores( :, positive_class_index ), ...
        classes( positive_class_index ), ...
        colors{ 1 } ...
        );
    opt_threshs{ 1 } = determine_optimal_threshold( roc_x, roc_y, thresholds, opts );
    plot_legend( axh, classes( positive_class_index ) );
end

end


function [ fh, axh ] = set_up_figure()

fh = figure( 'color', 'w' );
fh.Position = [ 10, 10, 960, 540 ];
axh = axes( fh );
axh.XTick = 0.0 : 0.1 : 1.0;
axh.YTick = 0.0 : 0.1 : 1.0;
axh.FontSize = 20;
axh.LineWidth = 2;
hold( axh, 'on' );
axis( 'square' );

end


function ph = plot_equality_line( axh )

ph = plot( axh, [ 0 1 ], [ 0 1 ] );
ph.LineStyle = ':';
ph.Color = 'k';
ph.LineWidth = 2;

end


function [ ph, roc_x, roc_y, thresholds, auc, opt ] = plot_roc_for_class( ...
    axh, ...
    labels, ...
    scores, ...
    class_value, ...
    color ...
    )

[ roc_x, roc_y, thresholds, auc, opt ] ...
    = perfcurve( labels, scores, class_value );
ph = plot( axh, roc_x, roc_y );
ph.Color = color;
ph.LineWidth = 2;

end


function optthresh = determine_optimal_threshold( ...
    roc_x, roc_y, ...
    thresholds, ...
    optpt ...
    )

optthresh = thresholds( ...
    ( roc_x == optpt( 1 ) ) ...
    & ( roc_y == optpt( 2 ) ) ...
    );

end


function lh = plot_legend( axh, classes )

stringy_classes = strsplit( num2str( classes ) );
lh = legend( ...
        axh, ...
        [ 'Equality', stringy_classes ] ...
        );
lh.Location = 'EastOutside';

end