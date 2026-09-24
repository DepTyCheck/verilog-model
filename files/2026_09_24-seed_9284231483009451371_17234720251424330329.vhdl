-- Seed: 9284231483009451371,17234720251424330329

entity exdpcjdwv is
  port (x : inout string(4 downto 4));
end exdpcjdwv;

architecture wmfniovgz of exdpcjdwv is
  
begin
  -- Single-driven assignments
  x <= x;
end wmfniovgz;

entity btrefvkqph is
  port (ffs : in bit_vector(4 downto 4); tie : linkage time);
end btrefvkqph;

architecture uv of btrefvkqph is
  
begin
  
end uv;

library ieee;
use ieee.std_logic_1164.all;

entity rxivjjwy is
  port (wlqfduhbon : buffer time; cwg : inout std_logic; qjnogsbkv : inout std_logic_vector(3 downto 1); fxo : buffer time);
end rxivjjwy;

architecture thnevpvpak of rxivjjwy is
  signal er : string(4 downto 4);
  signal ci : string(4 downto 4);
  signal zktimmag : time;
  signal qi : bit_vector(4 downto 4);
  signal mlj : string(4 downto 4);
begin
  p : entity work.exdpcjdwv
    port map (x => mlj);
  ghbr : entity work.btrefvkqph
    port map (ffs => qi, tie => zktimmag);
  ssr : entity work.exdpcjdwv
    port map (x => ci);
  rbxksgd : entity work.exdpcjdwv
    port map (x => er);
  
  -- Single-driven assignments
  qi <= qi;
  wlqfduhbon <= wlqfduhbon;
end thnevpvpak;

entity yplcqlxwl is
  port (ytrt : buffer real);
end yplcqlxwl;

library ieee;
use ieee.std_logic_1164.all;

architecture utbg of yplcqlxwl is
  signal ytwa : time;
  signal iyorr : std_logic_vector(3 downto 1);
  signal hu : std_logic;
  signal ssjq : time;
begin
  r : entity work.rxivjjwy
    port map (wlqfduhbon => ssjq, cwg => hu, qjnogsbkv => iyorr, fxo => ytwa);
end utbg;



-- Seed after: 11192014934981820735,17234720251424330329
