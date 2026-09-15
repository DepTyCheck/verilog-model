-- Seed: 11780670059052177190,13613332369802491303

library ieee;
use ieee.std_logic_1164.all;

entity uhtukfbes is
  port (xm : linkage std_logic_vector(2 downto 2); u : buffer time; gawrlvfnt : out real);
end uhtukfbes;

architecture hnyixqttna of uhtukfbes is
  
begin
  -- Single-driven assignments
  gawrlvfnt <= 2_2_1.04220;
  u <= u;
end hnyixqttna;

library ieee;
use ieee.std_logic_1164.all;

entity atgspazusp is
  port (nnxfxwgc : in integer; qd : buffer time_vector(2 to 1); fpn : in std_logic; yfbdlcj : out bit);
end atgspazusp;

library ieee;
use ieee.std_logic_1164.all;

architecture rkkwr of atgspazusp is
  signal cdkyanhxx : real;
  signal u : time;
  signal stjqkvxf : real;
  signal vsr : time;
  signal wvzuzt : std_logic_vector(2 downto 2);
begin
  alhyo : entity work.uhtukfbes
    port map (xm => wvzuzt, u => vsr, gawrlvfnt => stjqkvxf);
  zxue : entity work.uhtukfbes
    port map (xm => wvzuzt, u => u, gawrlvfnt => cdkyanhxx);
  
  -- Single-driven assignments
  yfbdlcj <= yfbdlcj;
  qd <= qd;
  
  -- Multi-driven assignments
  wvzuzt <= (others => 'U');
  wvzuzt <= "U";
  wvzuzt <= (others => 'H');
  wvzuzt <= wvzuzt;
end rkkwr;

library ieee;
use ieee.std_logic_1164.all;

entity lpvaesds is
  port (zlcqohqi : in std_logic);
end lpvaesds;

library ieee;
use ieee.std_logic_1164.all;

architecture thevadfdm of lpvaesds is
  signal fxbwfht : bit;
  signal slp : std_logic;
  signal amqfafd : time_vector(2 to 1);
  signal cncpnijz : integer;
  signal gmiyjd : real;
  signal gqbdqcbj : time;
  signal wwcwnhb : real;
  signal xmfzjmvcg : time;
  signal fygxbwm : real;
  signal ztuimurll : time;
  signal blsnfyxj : std_logic_vector(2 downto 2);
begin
  lfycmtlwl : entity work.uhtukfbes
    port map (xm => blsnfyxj, u => ztuimurll, gawrlvfnt => fygxbwm);
  mqfihwsin : entity work.uhtukfbes
    port map (xm => blsnfyxj, u => xmfzjmvcg, gawrlvfnt => wwcwnhb);
  qapzhk : entity work.uhtukfbes
    port map (xm => blsnfyxj, u => gqbdqcbj, gawrlvfnt => gmiyjd);
  gy : entity work.atgspazusp
    port map (nnxfxwgc => cncpnijz, qd => amqfafd, fpn => slp, yfbdlcj => fxbwfht);
  
  -- Single-driven assignments
  cncpnijz <= cncpnijz;
  
  -- Multi-driven assignments
  blsnfyxj <= blsnfyxj;
  slp <= zlcqohqi;
  blsnfyxj <= "X";
  blsnfyxj <= blsnfyxj;
end thevadfdm;

library ieee;
use ieee.std_logic_1164.all;

entity nfedrkecw is
  port (yahizrn : inout real; tlqxsulkhi : out time; y : in character; agpvmrr : out std_logic_vector(0 to 2));
end nfedrkecw;

library ieee;
use ieee.std_logic_1164.all;

architecture xgypqchhf of nfedrkecw is
  signal yswvav : std_logic;
  signal dptex : time;
  signal c : std_logic_vector(2 downto 2);
begin
  epncnutcdh : entity work.uhtukfbes
    port map (xm => c, u => dptex, gawrlvfnt => yahizrn);
  re : entity work.lpvaesds
    port map (zlcqohqi => yswvav);
  
  -- Multi-driven assignments
  agpvmrr <= ('H', 'H', '-');
  agpvmrr <= agpvmrr;
end xgypqchhf;



-- Seed after: 4662105520103095744,13613332369802491303
