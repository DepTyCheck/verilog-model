-- Seed: 14510888830714739263,5444032936641997477

library ieee;
use ieee.std_logic_1164.all;

entity mdltcm is
  port (t : buffer std_logic_vector(0 downto 3); qbnshik : inout real; vri : out integer_vector(3 to 2));
end mdltcm;

architecture m of mdltcm is
  
begin
  -- Single-driven assignments
  vri <= vri;
  qbnshik <= 16#0_7_B_4_D.2#;
end m;

entity fo is
  port (k : in character);
end fo;

library ieee;
use ieee.std_logic_1164.all;

architecture znbo of fo is
  signal qi : integer_vector(3 to 2);
  signal xbf : real;
  signal qvb : std_logic_vector(0 downto 3);
  signal uv : integer_vector(3 to 2);
  signal dwj : real;
  signal v : integer_vector(3 to 2);
  signal wbojwwj : real;
  signal vcpby : std_logic_vector(0 downto 3);
  signal skmx : integer_vector(3 to 2);
  signal zflqrwjlj : real;
  signal urwkw : std_logic_vector(0 downto 3);
begin
  vr : entity work.mdltcm
    port map (t => urwkw, qbnshik => zflqrwjlj, vri => skmx);
  hlfeu : entity work.mdltcm
    port map (t => vcpby, qbnshik => wbojwwj, vri => v);
  sqfpgnhfb : entity work.mdltcm
    port map (t => urwkw, qbnshik => dwj, vri => uv);
  ruljwivjvs : entity work.mdltcm
    port map (t => qvb, qbnshik => xbf, vri => qi);
  
  -- Multi-driven assignments
  urwkw <= (others => '0');
end znbo;



-- Seed after: 13704371070987330756,5444032936641997477
