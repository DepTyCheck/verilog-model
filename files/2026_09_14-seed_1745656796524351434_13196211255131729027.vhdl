-- Seed: 1745656796524351434,13196211255131729027

library ieee;
use ieee.std_logic_1164.all;

entity spevnu is
  port (boy : inout real; cfvsmuiet : buffer real; ijvo : in integer; mx : out std_logic);
end spevnu;

architecture bjmsqcz of spevnu is
  
begin
  -- Single-driven assignments
  cfvsmuiet <= 3_0_2_3_0.31243;
  boy <= 2#0.1#;
  
  -- Multi-driven assignments
  mx <= 'U';
  mx <= '1';
  mx <= mx;
end bjmsqcz;

library ieee;
use ieee.std_logic_1164.all;

entity ydmg is
  port (ikdvn : out std_logic; i : buffer boolean);
end ydmg;

architecture yb of ydmg is
  signal yexgy : integer;
  signal tckxpfog : real;
  signal vyn : real;
  signal nq : integer;
  signal iktxjalgh : real;
  signal lwuwhr : real;
begin
  ehrxdmttj : entity work.spevnu
    port map (boy => lwuwhr, cfvsmuiet => iktxjalgh, ijvo => nq, mx => ikdvn);
  whfbagxzdr : entity work.spevnu
    port map (boy => vyn, cfvsmuiet => tckxpfog, ijvo => yexgy, mx => ikdvn);
  
  -- Multi-driven assignments
  ikdvn <= '-';
  ikdvn <= '1';
end yb;

entity fi is
  port (wnucjqn : linkage boolean);
end fi;

library ieee;
use ieee.std_logic_1164.all;

architecture iofqdmmne of fi is
  signal kvchjkoznk : integer;
  signal xohb : real;
  signal kqbqrrpo : real;
  signal nscltqge : boolean;
  signal hq : std_logic;
begin
  ncel : entity work.ydmg
    port map (ikdvn => hq, i => nscltqge);
  c : entity work.spevnu
    port map (boy => kqbqrrpo, cfvsmuiet => xohb, ijvo => kvchjkoznk, mx => hq);
  
  -- Single-driven assignments
  kvchjkoznk <= 2_2_3;
  
  -- Multi-driven assignments
  hq <= 'Z';
  hq <= 'L';
  hq <= hq;
  hq <= hq;
end iofqdmmne;

library ieee;
use ieee.std_logic_1164.all;

entity w is
  port (idsti : linkage std_logic; hogv : buffer real_vector(1 to 0); dllmt : inout std_logic);
end w;

library ieee;
use ieee.std_logic_1164.all;

architecture nyponu of w is
  signal fjrt : boolean;
  signal fejdgwnn : std_logic;
  signal quf : integer;
  signal utdzgc : real;
  signal stjmlpv : real;
  signal wbtudvgtl : boolean;
  signal vav : std_logic;
  signal honnfp : integer;
  signal olcud : real;
  signal mijm : real;
begin
  yysek : entity work.spevnu
    port map (boy => mijm, cfvsmuiet => olcud, ijvo => honnfp, mx => vav);
  pwtfnooij : entity work.fi
    port map (wnucjqn => wbtudvgtl);
  owyzcnbmov : entity work.spevnu
    port map (boy => stjmlpv, cfvsmuiet => utdzgc, ijvo => quf, mx => fejdgwnn);
  pvggqwmk : entity work.fi
    port map (wnucjqn => fjrt);
  
  -- Single-driven assignments
  hogv <= (others => 0.0);
  honnfp <= honnfp;
  
  -- Multi-driven assignments
  dllmt <= dllmt;
  fejdgwnn <= dllmt;
  vav <= dllmt;
end nyponu;



-- Seed after: 11502680521905010006,13196211255131729027
