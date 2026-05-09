-- Seed: 4818895773410867855,11060400121348610183



entity qzyhbbv is
  port (cdwp : in character; puj : out integer; hbxokikmj : buffer integer);
end qzyhbbv;



architecture ldh of qzyhbbv is
  
begin
  
end ldh;



entity ljmru is
  port (xzikghj : in integer);
end ljmru;



architecture isfbdyl of ljmru is
  signal frcvkljiq : integer;
  signal odpaeo : integer;
  signal clxyu : character;
  signal ccfkzrc : integer;
  signal fxhbi : integer;
  signal y : character;
  signal eb : integer;
  signal vagxcin : integer;
  signal mml : character;
begin
  dlpxokqvv : entity work.qzyhbbv
    port map (cdwp => mml, puj => vagxcin, hbxokikmj => eb);
  hewlgok : entity work.qzyhbbv
    port map (cdwp => y, puj => fxhbi, hbxokikmj => ccfkzrc);
  xsunstddob : entity work.qzyhbbv
    port map (cdwp => clxyu, puj => odpaeo, hbxokikmj => frcvkljiq);
end isfbdyl;



entity vqcwkl is
  port (kras : linkage integer; tdv : linkage character; wwrmv : in integer; mq : inout bit);
end vqcwkl;



architecture ghrsluv of vqcwkl is
  signal hcuwydhm : integer;
  signal tsjxab : integer;
  signal uvlyfx : character;
begin
  qlcb : entity work.qzyhbbv
    port map (cdwp => uvlyfx, puj => tsjxab, hbxokikmj => hcuwydhm);
end ghrsluv;



entity mbycbepfp is
  port (xmatzje : in integer);
end mbycbepfp;



architecture ccsioob of mbycbepfp is
  signal ontoikp : bit;
  signal wxpsvabbod : integer;
  signal dt : integer;
  signal tgpo : integer;
  signal dvv : character;
  signal qvlonxo : integer;
begin
  fkzol : entity work.ljmru
    port map (xzikghj => qvlonxo);
  aynr : entity work.qzyhbbv
    port map (cdwp => dvv, puj => tgpo, hbxokikmj => qvlonxo);
  shbkulhkw : entity work.vqcwkl
    port map (kras => dt, tdv => dvv, wwrmv => wxpsvabbod, mq => ontoikp);
end ccsioob;



-- Seed after: 8806407764045366207,11060400121348610183
