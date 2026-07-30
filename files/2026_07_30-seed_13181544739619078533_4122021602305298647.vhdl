-- Seed: 13181544739619078533,4122021602305298647

entity elx is
  port (pv : buffer real);
end elx;

architecture urid of elx is
  
begin
  -- Single-driven assignments
  pv <= 8#1_1_3_7.0#;
end urid;

entity brhbbdmss is
  port (alqkiwlx : linkage string(4 to 5); ystzr : in time_vector(0 to 4));
end brhbbdmss;

architecture okeap of brhbbdmss is
  signal szmiln : real;
  signal nmdajmwvm : real;
begin
  scmd : entity work.elx
    port map (pv => nmdajmwvm);
  wzuhge : entity work.elx
    port map (pv => szmiln);
end okeap;

entity fbhm is
  port (m : out boolean; neftghis : inout integer_vector(1 to 4));
end fbhm;

architecture nsti of fbhm is
  signal wpialwn : time_vector(0 to 4);
  signal ukaari : string(4 to 5);
begin
  gdutgjczh : entity work.brhbbdmss
    port map (alqkiwlx => ukaari, ystzr => wpialwn);
  
  -- Single-driven assignments
  wpialwn <= (8#6_5# ps, 1.203 ns, 02321.0 us, 2_1_1_2_0.31000 ms, 3.2 ms);
  m <= TRUE;
  neftghis <= neftghis;
end nsti;



-- Seed after: 16894557629113611577,4122021602305298647
