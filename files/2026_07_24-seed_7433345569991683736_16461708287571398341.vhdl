-- Seed: 7433345569991683736,16461708287571398341

library ieee;
use ieee.std_logic_1164.all;

entity dg is
  port (sgogkv : inout std_logic_vector(3 downto 0); pnqbsjtptw : buffer std_logic_vector(1 downto 3); eshbzdnjbq : in bit_vector(1 to 1));
end dg;

architecture idpgep of dg is
  
begin
  -- Multi-driven assignments
  pnqbsjtptw <= (others => '0');
end idpgep;

library ieee;
use ieee.std_logic_1164.all;

entity wgt is
  port (elhtouzo : inout std_logic; v : linkage real; hwhzqueaz : out time);
end wgt;

library ieee;
use ieee.std_logic_1164.all;

architecture ktsprs of wgt is
  signal qbtwwvzhib : bit_vector(1 to 1);
  signal vurpv : std_logic_vector(1 downto 3);
  signal nzaflsffte : std_logic_vector(3 downto 0);
begin
  f : entity work.dg
    port map (sgogkv => nzaflsffte, pnqbsjtptw => vurpv, eshbzdnjbq => qbtwwvzhib);
  
  -- Single-driven assignments
  hwhzqueaz <= hwhzqueaz;
  qbtwwvzhib <= (others => '0');
end ktsprs;

entity qufxybjc is
  port (j : inout real; qdtqhrfhr : inout character);
end qufxybjc;

library ieee;
use ieee.std_logic_1164.all;

architecture a of qufxybjc is
  signal ltzhmv : bit_vector(1 to 1);
  signal zoclxnu : std_logic_vector(1 downto 3);
  signal u : bit_vector(1 to 1);
  signal tldgoynyws : std_logic_vector(1 downto 3);
  signal tpnyve : std_logic_vector(3 downto 0);
  signal egj : bit_vector(1 to 1);
  signal lxh : bit_vector(1 to 1);
  signal sfnxaz : std_logic_vector(1 downto 3);
  signal rl : std_logic_vector(3 downto 0);
begin
  bsnilbhwjy : entity work.dg
    port map (sgogkv => rl, pnqbsjtptw => sfnxaz, eshbzdnjbq => lxh);
  lnn : entity work.dg
    port map (sgogkv => rl, pnqbsjtptw => sfnxaz, eshbzdnjbq => egj);
  dtt : entity work.dg
    port map (sgogkv => tpnyve, pnqbsjtptw => tldgoynyws, eshbzdnjbq => u);
  yuieszyppn : entity work.dg
    port map (sgogkv => rl, pnqbsjtptw => zoclxnu, eshbzdnjbq => ltzhmv);
  
  -- Single-driven assignments
  u <= lxh;
  
  -- Multi-driven assignments
  zoclxnu <= sfnxaz;
  rl <= "L-LX";
end a;

library ieee;
use ieee.std_logic_1164.all;

entity pl is
  port (sztw : inout std_logic; o : linkage integer; bocvfdla : buffer std_logic_vector(3 to 1));
end pl;

architecture ixmmimt of pl is
  signal p : character;
  signal i : real;
  signal iczshfeoe : character;
  signal vhrvufj : real;
  signal fjghgxzmda : time;
  signal gms : real;
begin
  swxmhoylho : entity work.wgt
    port map (elhtouzo => sztw, v => gms, hwhzqueaz => fjghgxzmda);
  yieoh : entity work.qufxybjc
    port map (j => vhrvufj, qdtqhrfhr => iczshfeoe);
  xemci : entity work.qufxybjc
    port map (j => i, qdtqhrfhr => p);
end ixmmimt;



-- Seed after: 10728390688481948007,16461708287571398341
