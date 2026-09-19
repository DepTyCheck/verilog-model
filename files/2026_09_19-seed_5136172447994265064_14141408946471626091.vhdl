-- Seed: 5136172447994265064,14141408946471626091

entity vh is
  port (tu : linkage boolean_vector(1 to 4));
end vh;

architecture tdpl of vh is
  
begin
  
end tdpl;

entity yikditzwe is
  port (chuoqcpkrn : inout real; j : buffer boolean_vector(3 to 4));
end yikditzwe;

architecture t of yikditzwe is
  signal ij : boolean_vector(1 to 4);
begin
  bavk : entity work.vh
    port map (tu => ij);
  
  -- Single-driven assignments
  chuoqcpkrn <= 2#0_0.1_1_0_0#;
  j <= (TRUE, FALSE);
end t;



-- Seed after: 2536329944775621807,14141408946471626091
