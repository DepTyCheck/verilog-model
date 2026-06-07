-- Seed: 16534610517177738421,13903879141658024201



entity blcgiiaim is
  port (rzxhjuhr : in time; slzxzjrkj : out integer; laoctajx : inout time_vector(0 to 4));
end blcgiiaim;



architecture lgyz of blcgiiaim is
  
begin
  
end lgyz;



entity rcmayd is
  port (u : out real; xrpaaryy : buffer time);
end rcmayd;



architecture vpnj of rcmayd is
  signal rp : time_vector(0 to 4);
  signal ooa : integer;
  signal ienufh : time;
  signal vdl : time_vector(0 to 4);
  signal mf : integer;
  signal gn : time_vector(0 to 4);
  signal fiwgjq : integer;
  signal rcziseu : time;
begin
  pdawklizw : entity work.blcgiiaim
    port map (rzxhjuhr => rcziseu, slzxzjrkj => fiwgjq, laoctajx => gn);
  ttsqbf : entity work.blcgiiaim
    port map (rzxhjuhr => xrpaaryy, slzxzjrkj => mf, laoctajx => vdl);
  eweewapu : entity work.blcgiiaim
    port map (rzxhjuhr => ienufh, slzxzjrkj => ooa, laoctajx => rp);
end vpnj;



entity ucohdjya is
  port (q : inout time_vector(4 to 4); yiwrj : linkage time);
end ucohdjya;



architecture u of ucohdjya is
  signal chesub : time_vector(0 to 4);
  signal kmfznssda : integer;
  signal ksg : time;
  signal qpobt : time_vector(0 to 4);
  signal opeq : integer;
  signal vq : time;
begin
  viqyievkyg : entity work.blcgiiaim
    port map (rzxhjuhr => vq, slzxzjrkj => opeq, laoctajx => qpobt);
  k : entity work.blcgiiaim
    port map (rzxhjuhr => ksg, slzxzjrkj => kmfznssda, laoctajx => chesub);
end u;

library ieee;
use ieee.std_logic_1164.all;

entity d is
  port (klkioffjnl : buffer std_logic_vector(4 to 0); qxud : out std_logic_vector(2 downto 3); yicbwohym : out time_vector(3 to 4));
end d;



architecture c of d is
  signal fvcx : time_vector(0 to 4);
  signal ncedt : integer;
  signal cbyew : time;
  signal n : time_vector(4 to 4);
  signal htkfbk : time_vector(0 to 4);
  signal fkqliqsyrn : integer;
  signal qcksxxtwef : time;
begin
  rscocx : entity work.blcgiiaim
    port map (rzxhjuhr => qcksxxtwef, slzxzjrkj => fkqliqsyrn, laoctajx => htkfbk);
  sirztn : entity work.ucohdjya
    port map (q => n, yiwrj => cbyew);
  job : entity work.blcgiiaim
    port map (rzxhjuhr => qcksxxtwef, slzxzjrkj => ncedt, laoctajx => fvcx);
end c;



-- Seed after: 16860690204862068166,13903879141658024201
