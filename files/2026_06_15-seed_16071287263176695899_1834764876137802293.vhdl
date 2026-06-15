-- Seed: 16071287263176695899,1834764876137802293

entity tdlfsp is
  port (txuinapmg : out bit; exvljq : linkage real; otpbv : out integer_vector(0 downto 0));
end tdlfsp;

architecture zm of tdlfsp is
  
begin
  -- Single-driven assignments
  otpbv <= (others => 16#B_F_3_1#);
end zm;

entity jelrfq is
  port (o : buffer integer; aspnd : out real_vector(4 downto 2); zoeorpws : out bit);
end jelrfq;

architecture nfjgbkpkl of jelrfq is
  signal vfewgtw : integer_vector(0 downto 0);
  signal dypmfv : real;
begin
  swa : entity work.tdlfsp
    port map (txuinapmg => zoeorpws, exvljq => dypmfv, otpbv => vfewgtw);
  
  -- Single-driven assignments
  aspnd <= (8#4_0_0.7243#, 14014.2_4_0_4, 4.142);
  o <= 8#7#;
end nfjgbkpkl;

entity j is
  port (xhau : linkage real_vector(4 to 4));
end j;

architecture mauziy of j is
  signal ukj : bit;
  signal dvcrkr : real_vector(4 downto 2);
  signal gplbszzrot : integer;
  signal pkjimpzfg : integer_vector(0 downto 0);
  signal lnjylq : real;
  signal sdqabtpjh : bit;
  signal oaeqzrj : bit;
  signal nspzkmlwf : real_vector(4 downto 2);
  signal lrqeqbvob : integer;
  signal pyhlvbboc : integer_vector(0 downto 0);
  signal inheiw : real;
  signal f : bit;
begin
  mzolcwv : entity work.tdlfsp
    port map (txuinapmg => f, exvljq => inheiw, otpbv => pyhlvbboc);
  xayrzfteb : entity work.jelrfq
    port map (o => lrqeqbvob, aspnd => nspzkmlwf, zoeorpws => oaeqzrj);
  rod : entity work.tdlfsp
    port map (txuinapmg => sdqabtpjh, exvljq => lnjylq, otpbv => pkjimpzfg);
  znrycxvs : entity work.jelrfq
    port map (o => gplbszzrot, aspnd => dvcrkr, zoeorpws => ukj);
end mauziy;

entity zgfosdd is
  port (jpxp : in integer; ri : buffer integer);
end zgfosdd;

architecture iz of zgfosdd is
  signal rnb : real_vector(4 to 4);
begin
  xnffyvet : entity work.j
    port map (xhau => rnb);
  
  -- Single-driven assignments
  ri <= 4;
end iz;



-- Seed after: 459280932914260094,1834764876137802293
