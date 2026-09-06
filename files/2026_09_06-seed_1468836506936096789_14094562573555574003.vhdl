-- Seed: 1468836506936096789,14094562573555574003

entity ebqoov is
  port (jqmvu : inout integer; bcybjfhxs : buffer bit_vector(3 downto 0));
end ebqoov;

architecture ancxtdmsav of ebqoov is
  
begin
  -- Single-driven assignments
  bcybjfhxs <= ('0', '1', '0', '0');
  jqmvu <= 4;
end ancxtdmsav;

entity eksahqubb is
  port (xqaiz : linkage time; mrop : in bit_vector(3 to 2));
end eksahqubb;

architecture ovp of eksahqubb is
  signal aiecyhiv : bit_vector(3 downto 0);
  signal brgnoqh : integer;
  signal pzyiuigys : bit_vector(3 downto 0);
  signal ejesxyaqgb : integer;
  signal hb : bit_vector(3 downto 0);
  signal tcfpb : integer;
begin
  ubefkcsb : entity work.ebqoov
    port map (jqmvu => tcfpb, bcybjfhxs => hb);
  ehnstf : entity work.ebqoov
    port map (jqmvu => ejesxyaqgb, bcybjfhxs => pzyiuigys);
  swvus : entity work.ebqoov
    port map (jqmvu => brgnoqh, bcybjfhxs => aiecyhiv);
end ovp;



-- Seed after: 2930245840602575570,14094562573555574003
