-- Seed: 1095718064014882664,6176025545472508329

entity lr is
  port (gckk : buffer time; tzgykjdbcn : buffer boolean_vector(1 downto 4); teeaaxecj : out time);
end lr;

architecture u of lr is
  
begin
  -- Single-driven assignments
  tzgykjdbcn <= tzgykjdbcn;
  teeaaxecj <= teeaaxecj;
end u;

entity rbemppgoz is
  port (chmaivtyfo : inout severity_level; tou : buffer integer);
end rbemppgoz;

architecture qbql of rbemppgoz is
  signal xzreptlkis : time;
  signal glelgsia : boolean_vector(1 downto 4);
  signal slpqqvpej : time;
  signal nzc : time;
  signal awgspp : boolean_vector(1 downto 4);
  signal kmiutdbuk : time;
  signal qwo : time;
  signal mkovfp : boolean_vector(1 downto 4);
  signal nshc : time;
begin
  llst : entity work.lr
    port map (gckk => nshc, tzgykjdbcn => mkovfp, teeaaxecj => qwo);
  iszztwi : entity work.lr
    port map (gckk => kmiutdbuk, tzgykjdbcn => awgspp, teeaaxecj => nzc);
  uyyidmplrj : entity work.lr
    port map (gckk => slpqqvpej, tzgykjdbcn => glelgsia, teeaaxecj => xzreptlkis);
  
  -- Single-driven assignments
  tou <= 8#0_0_7#;
  chmaivtyfo <= NOTE;
end qbql;

entity svezmrld is
  port (toghtiop : in time);
end svezmrld;

architecture jcaj of svezmrld is
  signal jocjywzhr : integer;
  signal wsa : severity_level;
  signal jrslhktp : integer;
  signal sdrxsx : severity_level;
  signal vibyt : integer;
  signal kzgvffrr : severity_level;
begin
  ip : entity work.rbemppgoz
    port map (chmaivtyfo => kzgvffrr, tou => vibyt);
  ckuhex : entity work.rbemppgoz
    port map (chmaivtyfo => sdrxsx, tou => jrslhktp);
  gzbqhrz : entity work.rbemppgoz
    port map (chmaivtyfo => wsa, tou => jocjywzhr);
end jcaj;



-- Seed after: 17422626991905215902,6176025545472508329
