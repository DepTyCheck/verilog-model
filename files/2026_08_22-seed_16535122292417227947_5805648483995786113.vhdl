-- Seed: 16535122292417227947,5805648483995786113

entity jxpspzruzm is
  port (grpgojuud : out real; gkuv : in real; nthzilq : inout integer; vuzw : linkage severity_level);
end jxpspzruzm;

architecture qppjhr of jxpspzruzm is
  
begin
  -- Single-driven assignments
  grpgojuud <= grpgojuud;
  nthzilq <= nthzilq;
end qppjhr;

library ieee;
use ieee.std_logic_1164.all;

entity xups is
  port (dmfrztlhpm : inout std_logic; nmbol : linkage time; mupszvd : buffer std_logic_vector(3 to 3); ua : buffer std_logic);
end xups;

architecture rrkdw of xups is
  signal wfqkqjjd : severity_level;
  signal jsr : integer;
  signal ast : real;
  signal g : severity_level;
  signal x : integer;
  signal dr : real;
  signal ihoo : real;
  signal p : severity_level;
  signal mn : integer;
  signal obllnki : real;
  signal tmbkk : severity_level;
  signal wtnk : integer;
  signal ier : real;
  signal wyhjloldn : real;
begin
  cscoohxve : entity work.jxpspzruzm
    port map (grpgojuud => wyhjloldn, gkuv => ier, nthzilq => wtnk, vuzw => tmbkk);
  kozyg : entity work.jxpspzruzm
    port map (grpgojuud => ier, gkuv => obllnki, nthzilq => mn, vuzw => p);
  un : entity work.jxpspzruzm
    port map (grpgojuud => ihoo, gkuv => dr, nthzilq => x, vuzw => g);
  qfokulebg : entity work.jxpspzruzm
    port map (grpgojuud => dr, gkuv => ast, nthzilq => jsr, vuzw => wfqkqjjd);
  
  -- Single-driven assignments
  obllnki <= 413.2;
  ast <= 00.3413;
  
  -- Multi-driven assignments
  ua <= 'Z';
end rrkdw;

library ieee;
use ieee.std_logic_1164.all;

entity tal is
  port (xc : out real; ivzfrtjm : in integer; ydhe : in std_logic_vector(2 downto 0));
end tal;

architecture rhobknsoh of tal is
  signal fyzdghjzf : severity_level;
  signal ixcfmoc : integer;
  signal imeyg : real;
  signal jicxh : real;
  signal mvtw : severity_level;
  signal ikxhozshoj : integer;
  signal d : real;
begin
  gxnadeetc : entity work.jxpspzruzm
    port map (grpgojuud => xc, gkuv => d, nthzilq => ikxhozshoj, vuzw => mvtw);
  kd : entity work.jxpspzruzm
    port map (grpgojuud => jicxh, gkuv => imeyg, nthzilq => ixcfmoc, vuzw => fyzdghjzf);
  
  -- Single-driven assignments
  imeyg <= xc;
  d <= jicxh;
end rhobknsoh;

library ieee;
use ieee.std_logic_1164.all;

entity r is
  port (nl : buffer std_logic; ukfxpiwa : inout time; rsegu : out bit; wmvxh : inout real);
end r;

library ieee;
use ieee.std_logic_1164.all;

architecture bwv of r is
  signal rg : std_logic_vector(3 to 3);
  signal l : time;
  signal kyvmk : std_logic_vector(2 downto 0);
  signal lrleuympb : std_logic_vector(2 downto 0);
  signal mevc : integer;
  signal ygpvkww : real;
begin
  sq : entity work.tal
    port map (xc => ygpvkww, ivzfrtjm => mevc, ydhe => lrleuympb);
  sifoeatr : entity work.tal
    port map (xc => wmvxh, ivzfrtjm => mevc, ydhe => kyvmk);
  wa : entity work.xups
    port map (dmfrztlhpm => nl, nmbol => l, mupszvd => rg, ua => nl);
  
  -- Single-driven assignments
  mevc <= mevc;
  ukfxpiwa <= 4.1_2_0_3 us;
  rsegu <= '1';
  
  -- Multi-driven assignments
  kyvmk <= lrleuympb;
  kyvmk <= "W00";
  nl <= nl;
end bwv;



-- Seed after: 17529199140498384639,5805648483995786113
