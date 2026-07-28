-- Seed: 17857876352612182786,2511821214772927453

library ieee;
use ieee.std_logic_1164.all;

entity okaiprh is
  port (n : linkage std_logic_vector(1 downto 3); rfaex : buffer real; pqaqmpl : in real; mkydvojf : buffer integer);
end okaiprh;

architecture pbss of okaiprh is
  
begin
  -- Single-driven assignments
  mkydvojf <= 3_1_0;
  rfaex <= 8#3_7.6#;
end pbss;

entity x is
  port (zjefio : out integer; xpb : linkage character; tvjkkkcx : in integer; mattk : out real);
end x;

library ieee;
use ieee.std_logic_1164.all;

architecture cmbcmuvrvn of x is
  signal oatmwzuxe : std_logic_vector(1 downto 3);
begin
  qdkvglvqlj : entity work.okaiprh
    port map (n => oatmwzuxe, rfaex => mattk, pqaqmpl => mattk, mkydvojf => zjefio);
  
  -- Multi-driven assignments
  oatmwzuxe <= oatmwzuxe;
  oatmwzuxe <= "";
  oatmwzuxe <= oatmwzuxe;
end cmbcmuvrvn;

library ieee;
use ieee.std_logic_1164.all;

entity wyksx is
  port (gsgjy : out std_logic; myeda : out integer; z : linkage std_logic_vector(1 downto 3));
end wyksx;

library ieee;
use ieee.std_logic_1164.all;

architecture rboqlqm of wyksx is
  signal hs : integer;
  signal rb : character;
  signal jyzubzwxd : integer;
  signal qjpqbukyky : real;
  signal ykewnhoaxt : std_logic_vector(1 downto 3);
  signal sp : integer;
  signal caj : real;
  signal jfsrukairk : real;
  signal fcwppdoab : std_logic_vector(1 downto 3);
begin
  txiyk : entity work.okaiprh
    port map (n => fcwppdoab, rfaex => jfsrukairk, pqaqmpl => caj, mkydvojf => sp);
  tsxpyqiszw : entity work.okaiprh
    port map (n => ykewnhoaxt, rfaex => caj, pqaqmpl => qjpqbukyky, mkydvojf => myeda);
  gvjga : entity work.x
    port map (zjefio => jyzubzwxd, xpb => rb, tvjkkkcx => hs, mattk => qjpqbukyky);
  
  -- Single-driven assignments
  hs <= 43;
  
  -- Multi-driven assignments
  gsgjy <= gsgjy;
end rboqlqm;



-- Seed after: 3472509780875073845,2511821214772927453
