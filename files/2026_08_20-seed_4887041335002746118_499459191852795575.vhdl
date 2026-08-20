-- Seed: 4887041335002746118,499459191852795575

entity fixpoczm is
  port (ockpdyjhv : linkage integer_vector(1 downto 3); tukecqj : buffer integer);
end fixpoczm;

architecture nq of fixpoczm is
  
begin
  -- Single-driven assignments
  tukecqj <= 16#6_0_A#;
end nq;

entity uieptmrdz is
  port (fzjocgs : linkage severity_level);
end uieptmrdz;

architecture duleouxg of uieptmrdz is
  signal hppjrl : integer;
  signal nfzggbxde : integer_vector(1 downto 3);
begin
  c : entity work.fixpoczm
    port map (ockpdyjhv => nfzggbxde, tukecqj => hppjrl);
end duleouxg;

entity pwr is
  port (ctfhfzkvcg : in severity_level);
end pwr;

architecture mjpa of pwr is
  signal fdeo : severity_level;
  signal xcdhhzbpjl : integer;
  signal afhlxaynj : integer_vector(1 downto 3);
begin
  hcb : entity work.fixpoczm
    port map (ockpdyjhv => afhlxaynj, tukecqj => xcdhhzbpjl);
  tavayb : entity work.uieptmrdz
    port map (fzjocgs => fdeo);
end mjpa;



-- Seed after: 17089101271902941661,499459191852795575
