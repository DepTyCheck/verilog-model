-- Seed: 1430342387056905771,12260394286515585877

library ieee;
use ieee.std_logic_1164.all;

entity hwyokdup is
  port ( sgzxccibid : inout string(5 to 1)
  ; ijtqbmrue : buffer std_logic_vector(3 downto 2)
  ; jfx : buffer std_logic_vector(4 to 2)
  ; bgjhc : inout integer
  );
end hwyokdup;

architecture iwqaoxmcjf of hwyokdup is
  
begin
  -- Single-driven assignments
  sgzxccibid <= (others => ' ');
  bgjhc <= 2#1_0_0#;
  
  -- Multi-driven assignments
  jfx <= jfx;
  jfx <= (others => '0');
end iwqaoxmcjf;

library ieee;
use ieee.std_logic_1164.all;

entity edtxrtm is
  port (oir : out bit_vector(0 to 0); hx : buffer std_logic; gyoyoebm : in real; fio : inout time);
end edtxrtm;

library ieee;
use ieee.std_logic_1164.all;

architecture tvsjpkawhf of edtxrtm is
  signal zfgyyd : integer;
  signal dgrdvlgnd : string(5 to 1);
  signal mqwe : integer;
  signal buuomx : std_logic_vector(4 to 2);
  signal sv : std_logic_vector(3 downto 2);
  signal wexujlioks : string(5 to 1);
begin
  fmhbkpmoo : entity work.hwyokdup
    port map (sgzxccibid => wexujlioks, ijtqbmrue => sv, jfx => buuomx, bgjhc => mqwe);
  ezzaxb : entity work.hwyokdup
    port map (sgzxccibid => dgrdvlgnd, ijtqbmrue => sv, jfx => buuomx, bgjhc => zfgyyd);
  
  -- Multi-driven assignments
  hx <= 'H';
  sv <= "WX";
  buuomx <= (others => '0');
end tvsjpkawhf;

entity xwupale is
  port (scnud : in boolean; vzxflasm : out bit_vector(4 to 2); zz : inout time);
end xwupale;

library ieee;
use ieee.std_logic_1164.all;

architecture seom of xwupale is
  signal htwotrokqn : integer;
  signal hrijafptd : std_logic_vector(4 to 2);
  signal mgp : std_logic_vector(3 downto 2);
  signal iylqwhqo : string(5 to 1);
  signal fptlve : real;
  signal gjvuqjx : std_logic;
  signal onpreb : bit_vector(0 to 0);
begin
  iuw : entity work.edtxrtm
    port map (oir => onpreb, hx => gjvuqjx, gyoyoebm => fptlve, fio => zz);
  vbwi : entity work.hwyokdup
    port map (sgzxccibid => iylqwhqo, ijtqbmrue => mgp, jfx => hrijafptd, bgjhc => htwotrokqn);
  
  -- Single-driven assignments
  vzxflasm <= (others => '0');
  fptlve <= 2#01101.1_1_0#;
  
  -- Multi-driven assignments
  mgp <= mgp;
  gjvuqjx <= '-';
  gjvuqjx <= 'W';
  mgp <= mgp;
end seom;



-- Seed after: 13924955900145769278,12260394286515585877
