-- Seed: 11835301269244537602,8421704836678237495

entity llvi is
  port (qukpknv : in bit; lprlg : inout bit; uhsm : linkage time);
end llvi;

architecture w of llvi is
  
begin
  -- Single-driven assignments
  lprlg <= '1';
end w;

entity dfzj is
  port (skiaz : inout integer);
end dfzj;

architecture jio of dfzj is
  signal vtxfyyk : time;
  signal hkhn : bit;
  signal c : time;
  signal mhy : bit;
  signal uqz : bit;
  signal syhpsjbj : time;
  signal sth : bit;
  signal ijeohxff : bit;
begin
  qjaj : entity work.llvi
    port map (qukpknv => ijeohxff, lprlg => sth, uhsm => syhpsjbj);
  as : entity work.llvi
    port map (qukpknv => uqz, lprlg => mhy, uhsm => c);
  njqhqh : entity work.llvi
    port map (qukpknv => uqz, lprlg => hkhn, uhsm => vtxfyyk);
end jio;

entity ptxuixfbsv is
  port (ktqmjgqf : buffer integer);
end ptxuixfbsv;

architecture gg of ptxuixfbsv is
  signal rwqjottsu : time;
  signal p : bit;
begin
  ihzsca : entity work.dfzj
    port map (skiaz => ktqmjgqf);
  liphkgzdkt : entity work.llvi
    port map (qukpknv => p, lprlg => p, uhsm => rwqjottsu);
end gg;



-- Seed after: 2898250503100028993,8421704836678237495
