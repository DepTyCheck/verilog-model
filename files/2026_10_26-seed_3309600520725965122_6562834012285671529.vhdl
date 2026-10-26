-- Seed: 3309600520725965122,6562834012285671529

entity bagbdcj is
  port (wgzhnl : inout integer_vector(0 to 4));
end bagbdcj;

architecture cj of bagbdcj is
  
begin
  -- Single-driven assignments
  wgzhnl <= wgzhnl;
end cj;

library ieee;
use ieee.std_logic_1164.all;

entity lv is
  port (zpcuscfyxv : linkage integer_vector(0 to 4); phkdsh : out std_logic; sxmiproh : in boolean; wx : linkage time);
end lv;

architecture foqn of lv is
  signal cer : integer_vector(0 to 4);
  signal tkoxavy : integer_vector(0 to 4);
begin
  m : entity work.bagbdcj
    port map (wgzhnl => tkoxavy);
  pgdbe : entity work.bagbdcj
    port map (wgzhnl => cer);
  
  -- Multi-driven assignments
  phkdsh <= phkdsh;
  phkdsh <= phkdsh;
  phkdsh <= phkdsh;
  phkdsh <= 'W';
end foqn;

library ieee;
use ieee.std_logic_1164.all;

entity s is
  port (fyo : inout time; xmq : buffer std_logic_vector(2 downto 4); mdtur : out time);
end s;

library ieee;
use ieee.std_logic_1164.all;

architecture ovsvd of s is
  signal nggyztd : boolean;
  signal w : std_logic;
  signal r : integer_vector(0 to 4);
  signal uaesx : time;
  signal xvaybwjil : std_logic;
  signal ezhr : integer_vector(0 to 4);
  signal n : boolean;
  signal lagwkyfekn : std_logic;
  signal zntxmqepz : integer_vector(0 to 4);
begin
  ryxqdtlcqh : entity work.lv
    port map (zpcuscfyxv => zntxmqepz, phkdsh => lagwkyfekn, sxmiproh => n, wx => mdtur);
  bli : entity work.lv
    port map (zpcuscfyxv => ezhr, phkdsh => xvaybwjil, sxmiproh => n, wx => uaesx);
  l : entity work.lv
    port map (zpcuscfyxv => r, phkdsh => w, sxmiproh => nggyztd, wx => fyo);
  
  -- Single-driven assignments
  n <= nggyztd;
  
  -- Multi-driven assignments
  xmq <= (others => '0');
  lagwkyfekn <= 'H';
  xmq <= "";
end ovsvd;

library ieee;
use ieee.std_logic_1164.all;

entity kxywmtzmgs is
  port (fcymyhgrvz : linkage std_logic; apkvzeirn : in time; llrabcgcsc : out integer);
end kxywmtzmgs;

library ieee;
use ieee.std_logic_1164.all;

architecture che of kxywmtzmgs is
  signal blm : time;
  signal gfagtjb : std_logic_vector(2 downto 4);
  signal ravvbjsk : time;
  signal uvy : time;
  signal fy : std_logic_vector(2 downto 4);
  signal je : time;
  signal bl : integer_vector(0 to 4);
begin
  bbbhrensn : entity work.bagbdcj
    port map (wgzhnl => bl);
  ztkzewyat : entity work.s
    port map (fyo => je, xmq => fy, mdtur => uvy);
  zopnqgwbm : entity work.s
    port map (fyo => ravvbjsk, xmq => gfagtjb, mdtur => blm);
  
  -- Single-driven assignments
  llrabcgcsc <= llrabcgcsc;
end che;



-- Seed after: 586585742412955992,6562834012285671529
