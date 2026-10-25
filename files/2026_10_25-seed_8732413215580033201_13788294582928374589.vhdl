-- Seed: 8732413215580033201,13788294582928374589

entity dfa is
  port (y : in character; pk : buffer bit_vector(2 downto 1); hgdheaoh : in integer; srtvrskknc : inout bit);
end dfa;

architecture omaprbc of dfa is
  
begin
  -- Single-driven assignments
  srtvrskknc <= '1';
  pk <= ('0', '1');
end omaprbc;

entity gcg is
  port (k : buffer real; xire : in real; yp : out real);
end gcg;

architecture idd of gcg is
  signal aoylpjycm : bit;
  signal xwaairsbh : bit_vector(2 downto 1);
  signal qvgdocejj : character;
  signal fwjcrs : bit;
  signal mvteyllei : integer;
  signal wlow : bit_vector(2 downto 1);
  signal po : character;
begin
  a : entity work.dfa
    port map (y => po, pk => wlow, hgdheaoh => mvteyllei, srtvrskknc => fwjcrs);
  gqrlgxuy : entity work.dfa
    port map (y => qvgdocejj, pk => xwaairsbh, hgdheaoh => mvteyllei, srtvrskknc => aoylpjycm);
  
  -- Single-driven assignments
  yp <= yp;
  qvgdocejj <= po;
  mvteyllei <= 4_2_0;
  po <= 'd';
  k <= k;
end idd;

library ieee;
use ieee.std_logic_1164.all;

entity jzndy is
  port (bl : inout std_logic_vector(3 to 3));
end jzndy;

architecture pbreujqgnj of jzndy is
  signal n : bit;
  signal ubducm : integer;
  signal zlmko : bit_vector(2 downto 1);
  signal bjmvwfvp : character;
  signal jjvlkwtkxd : real;
  signal davcg : real;
begin
  aoepcu : entity work.gcg
    port map (k => davcg, xire => jjvlkwtkxd, yp => jjvlkwtkxd);
  kozhdu : entity work.dfa
    port map (y => bjmvwfvp, pk => zlmko, hgdheaoh => ubducm, srtvrskknc => n);
  
  -- Single-driven assignments
  ubducm <= 16#5#;
  bjmvwfvp <= bjmvwfvp;
  
  -- Multi-driven assignments
  bl <= "W";
  bl <= (others => 'H');
end pbreujqgnj;



-- Seed after: 7943167595557553440,13788294582928374589
