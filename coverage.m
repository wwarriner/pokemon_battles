coverage_table = zeros( max( combats{ :, 1 } ), max( combats{ :, 2 } ) );
for i = 1 : size( combats, 1 )
    coverage_table( combats{ i, 1 }, combats{ i, 2 } ) = 1;
end

symmetry = min( coverage_table, coverage_table.' );
symmetry_count = sum( symmetry( : ) );