-- Seed: 16226465952853864166,2338584220606314193

library ieee;
use ieee.std_logic_1164.all;

entity zcd is
  port (yqizhv : buffer std_logic_vector(2 downto 2); yvlhf : buffer severity_level; miznpx : in real; czzem : buffer std_logic);
end zcd;

architecture dembtdj of zcd is
  
begin
  -- Single-driven assignments
  yvlhf <= WARNING;
  
  -- Multi-driven assignments
  czzem <= 'X';
end dembtdj;

entity n is
  port (fhzgkqt : out real; mbbfavpkt : linkage character);
end n;

library ieee;
use ieee.std_logic_1164.all;

architecture kddqhnsp of n is
  signal aonfseu : real;
  signal jkcuj : severity_level;
  signal ow : real;
  signal kit : severity_level;
  signal uuob : std_logic_vector(2 downto 2);
  signal notaliild : severity_level;
  signal ieo : std_logic;
  signal nzkqrvhr : severity_level;
  signal wqflekcpvx : std_logic_vector(2 downto 2);
begin
  d : entity work.zcd
    port map (yqizhv => wqflekcpvx, yvlhf => nzkqrvhr, miznpx => fhzgkqt, czzem => ieo);
  mtts : entity work.zcd
    port map (yqizhv => wqflekcpvx, yvlhf => notaliild, miznpx => fhzgkqt, czzem => ieo);
  eirilusdnb : entity work.zcd
    port map (yqizhv => uuob, yvlhf => kit, miznpx => ow, czzem => ieo);
  pcnwa : entity work.zcd
    port map (yqizhv => wqflekcpvx, yvlhf => jkcuj, miznpx => aonfseu, czzem => ieo);
  
  -- Single-driven assignments
  fhzgkqt <= 8#2_0_7.0_1_6_6#;
  ow <= fhzgkqt;
  aonfseu <= aonfseu;
  
  -- Multi-driven assignments
  ieo <= 'L';
  wqflekcpvx <= "L";
  wqflekcpvx <= "L";
  uuob <= (others => 'L');
end kddqhnsp;

entity obkbb is
  port (axgcalfat : buffer time; enda : inout boolean_vector(3 to 2); jly : linkage boolean);
end obkbb;

library ieee;
use ieee.std_logic_1164.all;

architecture hqoq of obkbb is
  signal lsqgoqp : std_logic;
  signal zjohdqze : severity_level;
  signal lw : std_logic_vector(2 downto 2);
  signal g : character;
  signal zhxgcwdfv : real;
begin
  nebyghhvek : entity work.n
    port map (fhzgkqt => zhxgcwdfv, mbbfavpkt => g);
  oedilnt : entity work.zcd
    port map (yqizhv => lw, yvlhf => zjohdqze, miznpx => zhxgcwdfv, czzem => lsqgoqp);
  
  -- Single-driven assignments
  axgcalfat <= axgcalfat;
  enda <= enda;
  
  -- Multi-driven assignments
  lsqgoqp <= 'X';
  lw <= (others => 'Z');
end hqoq;



-- Seed after: 7325816449431188309,2338584220606314193
