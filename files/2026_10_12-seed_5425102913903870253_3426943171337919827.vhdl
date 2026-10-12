-- Seed: 5425102913903870253,3426943171337919827

entity udlcupop is
  port (iyj : out time_vector(0 downto 2); zrq : in bit_vector(3 downto 3));
end udlcupop;

architecture hihn of udlcupop is
  
begin
  
end hihn;

entity yafssdguh is
  port (desyfqi : buffer real);
end yafssdguh;

architecture csyshondi of yafssdguh is
  signal zhnsadcb : time_vector(0 downto 2);
  signal imtalxv : time_vector(0 downto 2);
  signal mauujouu : bit_vector(3 downto 3);
  signal prukdnb : time_vector(0 downto 2);
begin
  sutp : entity work.udlcupop
    port map (iyj => prukdnb, zrq => mauujouu);
  bptgomqes : entity work.udlcupop
    port map (iyj => imtalxv, zrq => mauujouu);
  i : entity work.udlcupop
    port map (iyj => zhnsadcb, zrq => mauujouu);
end csyshondi;

entity cibcg is
  port (fkeeqyzf : inout integer);
end cibcg;

architecture okttur of cibcg is
  signal nkmp : real;
  signal vlfdipevk : real;
  signal eixfwyikh : time_vector(0 downto 2);
  signal nqedw : bit_vector(3 downto 3);
  signal sf : time_vector(0 downto 2);
begin
  cvxl : entity work.udlcupop
    port map (iyj => sf, zrq => nqedw);
  iubfelcrpc : entity work.udlcupop
    port map (iyj => eixfwyikh, zrq => nqedw);
  mviqh : entity work.yafssdguh
    port map (desyfqi => vlfdipevk);
  webzcku : entity work.yafssdguh
    port map (desyfqi => nkmp);
  
  -- Single-driven assignments
  fkeeqyzf <= 16#B_6_2_3_E#;
  nqedw <= nqedw;
end okttur;



-- Seed after: 10439695682586694935,3426943171337919827
