-- Seed: 5879259964929299934,3687118713772291287

entity ir is
  port (wxncxljspf : out integer; kmgsus : linkage time; eedlo : linkage boolean_vector(4 to 2); gzeuag : buffer integer_vector(0 downto 1));
end ir;

architecture jsauleadzf of ir is
  
begin
  -- Single-driven assignments
  gzeuag <= (others => 0);
end jsauleadzf;

entity hbmyrqxgb is
  port (fkh : in real; p : buffer time; khiojy : buffer integer);
end hbmyrqxgb;

architecture mgg of hbmyrqxgb is
  signal wkn : integer_vector(0 downto 1);
  signal vqtg : boolean_vector(4 to 2);
  signal hxhje : integer;
  signal kvfwnubc : integer_vector(0 downto 1);
  signal ajzusykq : boolean_vector(4 to 2);
  signal iwqq : time;
  signal hxjjhrn : integer_vector(0 downto 1);
  signal pid : boolean_vector(4 to 2);
  signal ovjpyxpsc : time;
  signal agynqvm : integer;
begin
  vhe : entity work.ir
    port map (wxncxljspf => agynqvm, kmgsus => ovjpyxpsc, eedlo => pid, gzeuag => hxjjhrn);
  l : entity work.ir
    port map (wxncxljspf => khiojy, kmgsus => iwqq, eedlo => ajzusykq, gzeuag => kvfwnubc);
  ovxz : entity work.ir
    port map (wxncxljspf => hxhje, kmgsus => p, eedlo => vqtg, gzeuag => wkn);
end mgg;



-- Seed after: 17096778506125960638,3687118713772291287
