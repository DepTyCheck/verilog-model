-- Seed: 14392594656147187140,499459191852795575

entity qyvyjduwky is
  port (trmfaaqly : out time_vector(4 to 2));
end qyvyjduwky;

architecture wfukffrba of qyvyjduwky is
  
begin
  -- Single-driven assignments
  trmfaaqly <= trmfaaqly;
end wfukffrba;

entity cgcwib is
  port (waurcs : in time);
end cgcwib;

architecture oqlxuxknc of cgcwib is
  signal bp : time_vector(4 to 2);
begin
  tdpgkwkyvu : entity work.qyvyjduwky
    port map (trmfaaqly => bp);
end oqlxuxknc;

library ieee;
use ieee.std_logic_1164.all;

entity pdhykcbgqh is
  port (jpsaezyzo : buffer std_logic_vector(2 downto 0));
end pdhykcbgqh;

architecture bifs of pdhykcbgqh is
  signal gymde : time_vector(4 to 2);
  signal myicihlnqu : time;
  signal yaydesn : time;
  signal mnvd : time_vector(4 to 2);
begin
  vjvbwggz : entity work.qyvyjduwky
    port map (trmfaaqly => mnvd);
  okeliqq : entity work.cgcwib
    port map (waurcs => yaydesn);
  dxcu : entity work.cgcwib
    port map (waurcs => myicihlnqu);
  dhum : entity work.qyvyjduwky
    port map (trmfaaqly => gymde);
  
  -- Single-driven assignments
  yaydesn <= yaydesn;
  myicihlnqu <= yaydesn;
  
  -- Multi-driven assignments
  jpsaezyzo <= "110";
  jpsaezyzo <= ('U', '1', '-');
  jpsaezyzo <= jpsaezyzo;
  jpsaezyzo <= ('0', 'X', 'L');
end bifs;

library ieee;
use ieee.std_logic_1164.all;

entity joipei is
  port (ovjplk : in std_logic);
end joipei;

architecture smbglds of joipei is
  
begin
  
end smbglds;



-- Seed after: 12554837459399222615,499459191852795575
