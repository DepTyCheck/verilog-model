-- Seed: 6623903633191848074,14629254427735353553

library ieee;
use ieee.std_logic_1164.all;

entity a is
  port (crwginc : in std_logic; xfeoxcoite : out time; xuwiv : inout real);
end a;

architecture vvov of a is
  
begin
  -- Single-driven assignments
  xfeoxcoite <= 40134.3 fs;
  xuwiv <= 413.0_1;
end vvov;

library ieee;
use ieee.std_logic_1164.all;

entity y is
  port (elnyit : inout std_logic);
end y;

library ieee;
use ieee.std_logic_1164.all;

architecture cxip of y is
  signal pmnpjefecq : real;
  signal wz : time;
  signal fmymhuzto : std_logic;
  signal qo : real;
  signal vqg : time;
  signal lei : std_logic;
  signal ykwzmu : real;
  signal ciyfkh : time;
  signal muxvcfm : std_logic;
begin
  k : entity work.a
    port map (crwginc => muxvcfm, xfeoxcoite => ciyfkh, xuwiv => ykwzmu);
  hvyhclmw : entity work.a
    port map (crwginc => lei, xfeoxcoite => vqg, xuwiv => qo);
  iz : entity work.a
    port map (crwginc => fmymhuzto, xfeoxcoite => wz, xuwiv => pmnpjefecq);
  
  -- Multi-driven assignments
  muxvcfm <= 'Z';
  fmymhuzto <= '0';
end cxip;

library ieee;
use ieee.std_logic_1164.all;

entity x is
  port (bxkhgazvdz : in integer; feozvclclz : buffer std_logic);
end x;

library ieee;
use ieee.std_logic_1164.all;

architecture zd of x is
  signal csoeqiqei : std_logic;
  signal p : real;
  signal lzvn : time;
  signal jm : std_logic;
  signal ioa : real;
  signal qwyuvrnvgq : time;
  signal i : std_logic;
  signal vbf : real;
  signal vprwmijpg : time;
begin
  kt : entity work.a
    port map (crwginc => feozvclclz, xfeoxcoite => vprwmijpg, xuwiv => vbf);
  qry : entity work.a
    port map (crwginc => i, xfeoxcoite => qwyuvrnvgq, xuwiv => ioa);
  lbemzznqfy : entity work.a
    port map (crwginc => jm, xfeoxcoite => lzvn, xuwiv => p);
  nyrdsf : entity work.y
    port map (elnyit => csoeqiqei);
  
  -- Multi-driven assignments
  feozvclclz <= '-';
  feozvclclz <= '-';
  csoeqiqei <= '1';
  feozvclclz <= '0';
end zd;

library ieee;
use ieee.std_logic_1164.all;

entity dpafx is
  port (lwwwax : out std_logic; wjhz : out std_logic; eufrmugbm : inout integer);
end dpafx;

library ieee;
use ieee.std_logic_1164.all;

architecture nnvhiv of dpafx is
  signal jbjuelru : real;
  signal yp : time;
  signal wht : std_logic;
begin
  ryvkti : entity work.a
    port map (crwginc => wht, xfeoxcoite => yp, xuwiv => jbjuelru);
  
  -- Single-driven assignments
  eufrmugbm <= 16#088#;
  
  -- Multi-driven assignments
  lwwwax <= 'U';
  lwwwax <= 'H';
end nnvhiv;



-- Seed after: 15780302143963030003,14629254427735353553
