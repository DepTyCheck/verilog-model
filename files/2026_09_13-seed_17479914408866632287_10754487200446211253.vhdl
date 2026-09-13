-- Seed: 17479914408866632287,10754487200446211253

entity xqifdndwla is
  port (wxu : out time; sbnkcoqbi : inout integer);
end xqifdndwla;

architecture vwtko of xqifdndwla is
  
begin
  -- Single-driven assignments
  wxu <= wxu;
  sbnkcoqbi <= sbnkcoqbi;
end vwtko;

entity sz is
  port (yrrbydtx : out time_vector(1 downto 4));
end sz;

architecture clxi of sz is
  signal dfqghtzhqe : integer;
  signal sblyvuki : time;
begin
  exi : entity work.xqifdndwla
    port map (wxu => sblyvuki, sbnkcoqbi => dfqghtzhqe);
  
  -- Single-driven assignments
  yrrbydtx <= yrrbydtx;
end clxi;

entity fn is
  port (hjxwso : buffer integer; pdx : out severity_level);
end fn;

architecture seooz of fn is
  signal hkllnzmsac : time;
  signal tedkrefkie : integer;
  signal ciitj : time;
  signal hkoat : integer;
  signal mistbc : time;
  signal xpit : time_vector(1 downto 4);
begin
  kkmo : entity work.sz
    port map (yrrbydtx => xpit);
  i : entity work.xqifdndwla
    port map (wxu => mistbc, sbnkcoqbi => hkoat);
  jrzrls : entity work.xqifdndwla
    port map (wxu => ciitj, sbnkcoqbi => tedkrefkie);
  pyeklr : entity work.xqifdndwla
    port map (wxu => hkllnzmsac, sbnkcoqbi => hjxwso);
  
  -- Single-driven assignments
  pdx <= WARNING;
end seooz;



-- Seed after: 3645338736548518420,10754487200446211253
