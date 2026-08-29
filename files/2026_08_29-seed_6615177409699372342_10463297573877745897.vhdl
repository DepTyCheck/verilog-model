-- Seed: 6615177409699372342,10463297573877745897

entity lecsjr is
  port (cjjql : in real_vector(2 downto 1); keunsjwb : in real_vector(4 downto 3); wgche : out integer; rberzniuf : inout time);
end lecsjr;

architecture jevjo of lecsjr is
  
begin
  
end jevjo;

entity sbhzhhk is
  port (afivpzodrj : in real; qksjilui : buffer time; huaagomm : in bit_vector(3 downto 1); plb : buffer real);
end sbhzhhk;

architecture ts of sbhzhhk is
  signal vfdnjz : time;
  signal d : integer;
  signal hko : real_vector(4 downto 3);
  signal sehtl : real_vector(2 downto 1);
  signal byoamyas : integer;
  signal gkjgpzsdcf : real_vector(4 downto 3);
  signal srqjcekjym : time;
  signal qtydtycwch : integer;
  signal ojnavtyaf : real_vector(4 downto 3);
  signal wly : time;
  signal jv : integer;
  signal hb : real_vector(2 downto 1);
  signal lgyroa : real_vector(2 downto 1);
begin
  jgkbfg : entity work.lecsjr
    port map (cjjql => lgyroa, keunsjwb => hb, wgche => jv, rberzniuf => wly);
  aosmsik : entity work.lecsjr
    port map (cjjql => hb, keunsjwb => ojnavtyaf, wgche => qtydtycwch, rberzniuf => srqjcekjym);
  hzh : entity work.lecsjr
    port map (cjjql => lgyroa, keunsjwb => gkjgpzsdcf, wgche => byoamyas, rberzniuf => qksjilui);
  iehr : entity work.lecsjr
    port map (cjjql => sehtl, keunsjwb => hko, wgche => d, rberzniuf => vfdnjz);
end ts;

entity kfowbnanw is
  port (ibrdu : inout severity_level; njm : buffer real; gvygvfkpjo : linkage severity_level; xgutqtzic : linkage boolean);
end kfowbnanw;

architecture dkavt of kfowbnanw is
  signal tlgu : time;
  signal ml : integer;
  signal qfyrawdhf : time;
  signal yd : integer;
  signal deg : real_vector(4 downto 3);
begin
  ljxdrahnhc : entity work.lecsjr
    port map (cjjql => deg, keunsjwb => deg, wgche => yd, rberzniuf => qfyrawdhf);
  tcrf : entity work.lecsjr
    port map (cjjql => deg, keunsjwb => deg, wgche => ml, rberzniuf => tlgu);
  
  -- Single-driven assignments
  njm <= njm;
  deg <= (0_2.0_0_0_0_3, 8#6671.4_7_7_2_0#);
  ibrdu <= WARNING;
end dkavt;

entity npllqqhja is
  port (p : out boolean);
end npllqqhja;

architecture uy of npllqqhja is
  signal ygbk : boolean;
  signal ubqbapyj : severity_level;
  signal k : real;
  signal obmmejy : severity_level;
  signal dweomm : boolean;
  signal z : severity_level;
  signal yxyauujan : real;
  signal tytvwaw : severity_level;
  signal erfhrzaqvz : time;
  signal ejbpecnc : integer;
  signal syot : real_vector(4 downto 3);
  signal zcguy : real_vector(2 downto 1);
begin
  hkch : entity work.lecsjr
    port map (cjjql => zcguy, keunsjwb => syot, wgche => ejbpecnc, rberzniuf => erfhrzaqvz);
  jugxafv : entity work.kfowbnanw
    port map (ibrdu => tytvwaw, njm => yxyauujan, gvygvfkpjo => z, xgutqtzic => dweomm);
  meplzyx : entity work.kfowbnanw
    port map (ibrdu => obmmejy, njm => k, gvygvfkpjo => ubqbapyj, xgutqtzic => ygbk);
  
  -- Single-driven assignments
  zcguy <= (1_3.21, 2_0_1_0_4.0_2_0);
  syot <= (2#0_1_0_0_0.11#, 8#511.2030#);
  p <= p;
end uy;



-- Seed after: 4565867523860317749,10463297573877745897
