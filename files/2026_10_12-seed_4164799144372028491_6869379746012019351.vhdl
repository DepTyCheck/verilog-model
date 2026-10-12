-- Seed: 4164799144372028491,6869379746012019351

entity qxbgjqphqp is
  port (jwt : buffer real; uwvglez : in integer; taizyezl : buffer boolean_vector(3 downto 0); q : out time_vector(3 downto 0));
end qxbgjqphqp;

architecture tvxryfnml of qxbgjqphqp is
  
begin
  -- Single-driven assignments
  jwt <= 03.0_0_4_2;
  taizyezl <= taizyezl;
  q <= (30.0433 us, 16#0_5_4_F_7.3D4D7# fs, 16#D_E# ps, 16#F_E_0_8_9.A_3_2# ps);
end tvxryfnml;

entity fh is
  port (ycfdlipxsn : buffer severity_level; d : in real; oiqajnh : out real; zn : in integer);
end fh;

architecture aox of fh is
  signal awapjio : time_vector(3 downto 0);
  signal prejezoc : boolean_vector(3 downto 0);
  signal zar : real;
  signal lqnwifqgt : time_vector(3 downto 0);
  signal kmw : boolean_vector(3 downto 0);
begin
  kfuzwloj : entity work.qxbgjqphqp
    port map (jwt => oiqajnh, uwvglez => zn, taizyezl => kmw, q => lqnwifqgt);
  zga : entity work.qxbgjqphqp
    port map (jwt => zar, uwvglez => zn, taizyezl => prejezoc, q => awapjio);
  
  -- Single-driven assignments
  ycfdlipxsn <= NOTE;
end aox;



-- Seed after: 17884488907646595258,6869379746012019351
