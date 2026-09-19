-- Seed: 14927309991516652255,14141408946471626091

entity dmtyli is
  port (wzrn : out integer; uowh : linkage bit_vector(4 downto 1); ymjm : linkage time; dyxsfxk : out time);
end dmtyli;

architecture o of dmtyli is
  
begin
  -- Single-driven assignments
  wzrn <= 2#1#;
  dyxsfxk <= dyxsfxk;
end o;

entity e is
  port (mpq : out real; reqsq : linkage integer_vector(1 to 0));
end e;

architecture odd of e is
  signal uql : time;
  signal vxdtwhzq : time;
  signal vsrsnpuqbt : bit_vector(4 downto 1);
  signal ge : integer;
  signal kp : time;
  signal ssukgw : time;
  signal sdubm : bit_vector(4 downto 1);
  signal wtyvr : integer;
  signal oapikz : time;
  signal cmvq : time;
  signal vrg : bit_vector(4 downto 1);
  signal mnhqis : integer;
begin
  fxusxvnpuj : entity work.dmtyli
    port map (wzrn => mnhqis, uowh => vrg, ymjm => cmvq, dyxsfxk => oapikz);
  yqa : entity work.dmtyli
    port map (wzrn => wtyvr, uowh => sdubm, ymjm => ssukgw, dyxsfxk => kp);
  prtalk : entity work.dmtyli
    port map (wzrn => ge, uowh => vsrsnpuqbt, ymjm => vxdtwhzq, dyxsfxk => uql);
end odd;



-- Seed after: 14429704400217588669,14141408946471626091
