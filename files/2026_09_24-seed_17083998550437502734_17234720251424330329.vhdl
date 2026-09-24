-- Seed: 17083998550437502734,17234720251424330329

entity nlvo is
  port (koozj : in real_vector(3 downto 0); nctynrrg : inout real_vector(3 to 2));
end nlvo;

architecture g of nlvo is
  
begin
  
end g;

entity hultcntdca is
  port (gmqxn : buffer integer);
end hultcntdca;

architecture yekbvjhta of hultcntdca is
  signal ypjtasbtmf : real_vector(3 to 2);
  signal banhf : real_vector(3 to 2);
  signal gwkjvhg : real_vector(3 downto 0);
  signal rwwyhqd : real_vector(3 to 2);
  signal pipund : real_vector(3 downto 0);
begin
  cjjvy : entity work.nlvo
    port map (koozj => pipund, nctynrrg => rwwyhqd);
  pjymkqs : entity work.nlvo
    port map (koozj => gwkjvhg, nctynrrg => banhf);
  qmvnajc : entity work.nlvo
    port map (koozj => pipund, nctynrrg => ypjtasbtmf);
  
  -- Single-driven assignments
  gmqxn <= 033;
end yekbvjhta;

entity bmr is
  port (ez : inout integer; iaanvx : inout time_vector(0 to 1));
end bmr;

architecture xh of bmr is
  signal lyhjancdkd : real_vector(3 to 2);
  signal gtc : real_vector(3 downto 0);
  signal csagbf : real_vector(3 to 2);
  signal yzqwraeibf : real_vector(3 to 2);
  signal hq : real_vector(3 downto 0);
begin
  gaozcg : entity work.nlvo
    port map (koozj => hq, nctynrrg => yzqwraeibf);
  ix : entity work.nlvo
    port map (koozj => hq, nctynrrg => csagbf);
  hzyb : entity work.nlvo
    port map (koozj => gtc, nctynrrg => lyhjancdkd);
  
  -- Single-driven assignments
  iaanvx <= (0 sec, 16#C0985.C_E_F# us);
  gtc <= (8#3.20264#, 133.3_3_4, 1_4.142, 2.222);
  ez <= 8#4_0_3_7_3#;
  hq <= hq;
end xh;



-- Seed after: 9468635278377745620,17234720251424330329
