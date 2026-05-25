-- Seed: 17165322482783161039,12431929723978769775



entity qosx is
  port (dbenxplwgu : inout time; upd : linkage real);
end qosx;



architecture eesvjv of qosx is
  
begin
  
end eesvjv;

library ieee;
use ieee.std_logic_1164.all;

entity ikaydz is
  port (vfkhq : inout real; vczev : inout std_logic; uo : out time; nfjwhy : out std_logic_vector(3 to 3));
end ikaydz;



architecture bft of ikaydz is
  signal uhpgjk : time;
  signal qvt : time;
  signal zu : time;
begin
  d : entity work.qosx
    port map (dbenxplwgu => zu, upd => vfkhq);
  jokub : entity work.qosx
    port map (dbenxplwgu => qvt, upd => vfkhq);
  f : entity work.qosx
    port map (dbenxplwgu => uhpgjk, upd => vfkhq);
  gxeqq : entity work.qosx
    port map (dbenxplwgu => uo, upd => vfkhq);
end bft;



entity miwqlaj is
  port (tmenva : inout boolean);
end miwqlaj;



architecture wv of miwqlaj is
  signal khhytgx : real;
  signal kty : time;
begin
  alytqtpyrk : entity work.qosx
    port map (dbenxplwgu => kty, upd => khhytgx);
end wv;



entity exilrrvzzn is
  port (sbopqwoz : inout real; tqzocih : buffer time_vector(0 downto 4); ne : linkage integer);
end exilrrvzzn;



architecture xdplatchv of exilrrvzzn is
  signal ul : boolean;
  signal cnwjfh : time;
  signal mjomefi : time;
begin
  sirt : entity work.qosx
    port map (dbenxplwgu => mjomefi, upd => sbopqwoz);
  mdetb : entity work.qosx
    port map (dbenxplwgu => cnwjfh, upd => sbopqwoz);
  hqmrnwxb : entity work.miwqlaj
    port map (tmenva => ul);
end xdplatchv;



-- Seed after: 2082311517857807672,12431929723978769775
