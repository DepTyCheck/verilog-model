-- Seed: 2594940017320814927,8927267689619684183

entity agbhpnfha is
  port (y : out integer; uuzdztsc : in bit_vector(2 downto 4));
end agbhpnfha;

architecture thtzbzkb of agbhpnfha is
  
begin
  -- Single-driven assignments
  y <= y;
end thtzbzkb;

entity wurv is
  port (jdlg : buffer integer_vector(4 downto 4); isvqha : inout boolean_vector(4 to 1));
end wurv;

architecture ktok of wurv is
  
begin
  -- Single-driven assignments
  isvqha <= (others => TRUE);
  jdlg <= jdlg;
end ktok;

entity ncqub is
  port (yjxqcew : linkage string(5 downto 4); z : in integer; hvzcgbcf : out real; zb : inout integer);
end ncqub;

architecture gqjyojjz of ncqub is
  signal rgfb : boolean_vector(4 to 1);
  signal fw : integer_vector(4 downto 4);
  signal ib : bit_vector(2 downto 4);
  signal sl : integer;
begin
  q : entity work.agbhpnfha
    port map (y => sl, uuzdztsc => ib);
  qfro : entity work.wurv
    port map (jdlg => fw, isvqha => rgfb);
  
  -- Single-driven assignments
  zb <= zb;
  ib <= (others => '0');
  hvzcgbcf <= hvzcgbcf;
end gqjyojjz;



-- Seed after: 8421059996156292083,8927267689619684183
