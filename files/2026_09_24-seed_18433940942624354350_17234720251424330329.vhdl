-- Seed: 18433940942624354350,17234720251424330329

entity moyswn is
  port (kpshrbrxlj : linkage real; dfjvnsdej : linkage integer_vector(3 to 0); gs : in severity_level);
end moyswn;

architecture lirshj of moyswn is
  
begin
  
end lirshj;

entity enxwdkfded is
  port (gkxtrjapnq : buffer integer; fp : out real_vector(2 downto 1));
end enxwdkfded;

architecture gyjfx of enxwdkfded is
  signal nsdkha : severity_level;
  signal otjb : integer_vector(3 to 0);
  signal h : real;
  signal n : integer_vector(3 to 0);
  signal j : real;
  signal butmsdlsiy : severity_level;
  signal mm : integer_vector(3 to 0);
  signal hjo : real;
begin
  gto : entity work.moyswn
    port map (kpshrbrxlj => hjo, dfjvnsdej => mm, gs => butmsdlsiy);
  itozolnzr : entity work.moyswn
    port map (kpshrbrxlj => j, dfjvnsdej => n, gs => butmsdlsiy);
  regkjb : entity work.moyswn
    port map (kpshrbrxlj => h, dfjvnsdej => otjb, gs => nsdkha);
  
  -- Single-driven assignments
  nsdkha <= butmsdlsiy;
  butmsdlsiy <= WARNING;
  gkxtrjapnq <= 8#6#;
  fp <= (2#0_0_0_0_1.1011#, 1_1_2.14402);
end gyjfx;



-- Seed after: 9006287631386960048,17234720251424330329
