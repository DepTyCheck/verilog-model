-- Seed: 10725876931693378894,511364357853360275

entity w is
  port (wubptcgpw : buffer time_vector(3 to 1));
end w;

architecture lxyeozpdxs of w is
  
begin
  -- Single-driven assignments
  wubptcgpw <= (others => 0 ns);
end lxyeozpdxs;

entity fbsngtnuy is
  port (wyefe : linkage integer_vector(4 to 0); ghwqci : linkage real; ruw : in real);
end fbsngtnuy;

architecture enb of fbsngtnuy is
  signal q : time_vector(3 to 1);
  signal wydraxrg : time_vector(3 to 1);
  signal t : time_vector(3 to 1);
begin
  pbbuchkbw : entity work.w
    port map (wubptcgpw => t);
  gpvtd : entity work.w
    port map (wubptcgpw => wydraxrg);
  hf : entity work.w
    port map (wubptcgpw => q);
end enb;

entity ypfugcz is
  port (tiiyfazt : out time);
end ypfugcz;

architecture stixppc of ypfugcz is
  signal ujxr : real;
  signal a : integer_vector(4 to 0);
  signal ffqy : real;
  signal vlkw : real;
  signal clalzibl : integer_vector(4 to 0);
begin
  pbjsheuhn : entity work.fbsngtnuy
    port map (wyefe => clalzibl, ghwqci => vlkw, ruw => ffqy);
  zlsn : entity work.fbsngtnuy
    port map (wyefe => a, ghwqci => ffqy, ruw => ujxr);
  
  -- Single-driven assignments
  tiiyfazt <= tiiyfazt;
  ujxr <= 16#A_E_7_7.37319#;
end stixppc;

entity tzubi is
  port (bqnu : buffer character);
end tzubi;

architecture b of tzubi is
  signal vuxyqm : time_vector(3 to 1);
  signal dpcauiwq : time_vector(3 to 1);
  signal oxgaljgwg : time_vector(3 to 1);
  signal pqbtnm : time_vector(3 to 1);
begin
  hs : entity work.w
    port map (wubptcgpw => pqbtnm);
  yp : entity work.w
    port map (wubptcgpw => oxgaljgwg);
  pjh : entity work.w
    port map (wubptcgpw => dpcauiwq);
  xnzthlnuy : entity work.w
    port map (wubptcgpw => vuxyqm);
end b;



-- Seed after: 8584267208445724674,511364357853360275
