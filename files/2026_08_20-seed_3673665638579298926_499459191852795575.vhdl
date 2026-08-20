-- Seed: 3673665638579298926,499459191852795575

library ieee;
use ieee.std_logic_1164.all;

entity nco is
  port (gkcebswepz : out std_logic_vector(3 to 3); sgpus : buffer time; zyvwjlpeu : buffer integer_vector(4 to 2));
end nco;

architecture iokwjybnkz of nco is
  
begin
  -- Single-driven assignments
  sgpus <= 16#4084C# ps;
  zyvwjlpeu <= (others => 0);
  
  -- Multi-driven assignments
  gkcebswepz <= gkcebswepz;
  gkcebswepz <= (others => 'X');
  gkcebswepz <= gkcebswepz;
  gkcebswepz <= gkcebswepz;
end iokwjybnkz;

library ieee;
use ieee.std_logic_1164.all;

entity janzbjjt is
  port (rzv : in integer; eg : linkage std_logic_vector(1 downto 2); gscsr : inout bit);
end janzbjjt;

architecture mnusye of janzbjjt is
  
begin
  -- Single-driven assignments
  gscsr <= '1';
end mnusye;

entity ysmaxkg is
  port (n : buffer character; qu : out integer);
end ysmaxkg;

library ieee;
use ieee.std_logic_1164.all;

architecture brnf of ysmaxkg is
  signal obggsb : integer_vector(4 to 2);
  signal euxtypes : time;
  signal bueotk : std_logic_vector(3 to 3);
  signal audwpe : integer_vector(4 to 2);
  signal zlfhou : time;
  signal meo : std_logic_vector(3 to 3);
begin
  jo : entity work.nco
    port map (gkcebswepz => meo, sgpus => zlfhou, zyvwjlpeu => audwpe);
  zqcekxrpre : entity work.nco
    port map (gkcebswepz => bueotk, sgpus => euxtypes, zyvwjlpeu => obggsb);
  
  -- Single-driven assignments
  qu <= qu;
  
  -- Multi-driven assignments
  meo <= meo;
  bueotk <= meo;
end brnf;

entity yd is
  port (tnfimabbsu : out character);
end yd;

library ieee;
use ieee.std_logic_1164.all;

architecture gkcfnea of yd is
  signal neqexep : integer_vector(4 to 2);
  signal anulx : time;
  signal ajnihjpse : integer_vector(4 to 2);
  signal l : time;
  signal qplrevy : integer_vector(4 to 2);
  signal duijwbpi : time;
  signal ohsd : integer_vector(4 to 2);
  signal qilsnqmeyi : time;
  signal viqp : std_logic_vector(3 to 3);
begin
  thfp : entity work.nco
    port map (gkcebswepz => viqp, sgpus => qilsnqmeyi, zyvwjlpeu => ohsd);
  gjppb : entity work.nco
    port map (gkcebswepz => viqp, sgpus => duijwbpi, zyvwjlpeu => qplrevy);
  vkk : entity work.nco
    port map (gkcebswepz => viqp, sgpus => l, zyvwjlpeu => ajnihjpse);
  t : entity work.nco
    port map (gkcebswepz => viqp, sgpus => anulx, zyvwjlpeu => neqexep);
  
  -- Multi-driven assignments
  viqp <= "1";
  viqp <= (others => '-');
  viqp <= (others => 'H');
end gkcfnea;



-- Seed after: 2896705960057540729,499459191852795575
