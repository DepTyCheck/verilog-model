-- Seed: 11858747728760937024,6562834012285671529

entity eqcn is
  port (zszbh : in time_vector(2 to 4); kyiwfdaa : in real; kwjfom : inout integer);
end eqcn;

architecture jkgxbzl of eqcn is
  
begin
  -- Single-driven assignments
  kwjfom <= 8#3_0_6_2#;
end jkgxbzl;

entity dqbsjiho is
  port (ep : linkage time; acehsnbbcl : linkage time; rhwdqqh : inout real; azllxhff : out real);
end dqbsjiho;

architecture gv of dqbsjiho is
  signal lceaxlc : integer;
  signal ac : time_vector(2 to 4);
begin
  fzuskju : entity work.eqcn
    port map (zszbh => ac, kyiwfdaa => rhwdqqh, kwjfom => lceaxlc);
  
  -- Single-driven assignments
  rhwdqqh <= 16#32.F_9_5_E#;
  ac <= (3 min, 16#3.0_E_8_C_8# ms, 8#2_5_1_1_3.74305# ps);
  azllxhff <= 16#E_1_4_4.0EE9#;
end gv;



-- Seed after: 7120805330742801554,6562834012285671529
