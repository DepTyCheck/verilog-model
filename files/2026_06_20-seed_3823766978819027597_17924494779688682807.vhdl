-- Seed: 3823766978819027597,17924494779688682807

library ieee;
use ieee.std_logic_1164.all;

entity zhbomyx is
  port (no : in time; j : linkage real; d : out std_logic_vector(2 to 3); smqfu : out std_logic_vector(2 downto 3));
end zhbomyx;

architecture octmwarlh of zhbomyx is
  
begin
  -- Multi-driven assignments
  smqfu <= (others => '0');
end octmwarlh;

entity lsg is
  port (cohkpiug : in integer_vector(3 to 0));
end lsg;

library ieee;
use ieee.std_logic_1164.all;

architecture hwprbjg of lsg is
  signal fhbuve : std_logic_vector(2 downto 3);
  signal ekyz : std_logic_vector(2 to 3);
  signal ybinsnp : real;
  signal dwmiyjrjvb : time;
begin
  ipjdzmsjtb : entity work.zhbomyx
    port map (no => dwmiyjrjvb, j => ybinsnp, d => ekyz, smqfu => fhbuve);
  
  -- Single-driven assignments
  dwmiyjrjvb <= 16#5_6_1_4_3# ns;
  
  -- Multi-driven assignments
  ekyz <= ('Z', '1');
  fhbuve <= (others => '0');
end hwprbjg;

library ieee;
use ieee.std_logic_1164.all;

entity sijtdzmok is
  port (bnry : inout std_logic);
end sijtdzmok;

library ieee;
use ieee.std_logic_1164.all;

architecture mqhnbjsq of sijtdzmok is
  signal edpayehcp : std_logic_vector(2 to 3);
  signal kyj : real;
  signal bef : time;
  signal ptegqola : std_logic_vector(2 downto 3);
  signal ygv : std_logic_vector(2 to 3);
  signal xfdypkhd : real;
  signal c : time;
begin
  nvtnmb : entity work.zhbomyx
    port map (no => c, j => xfdypkhd, d => ygv, smqfu => ptegqola);
  vdbeusjz : entity work.zhbomyx
    port map (no => bef, j => kyj, d => edpayehcp, smqfu => ptegqola);
  
  -- Single-driven assignments
  c <= 4_1.32412 ns;
  bef <= 2 sec;
  
  -- Multi-driven assignments
  bnry <= 'H';
  bnry <= 'W';
  bnry <= '0';
  bnry <= 'X';
end mqhnbjsq;

entity hwumn is
  port (x : buffer real; i : out real; gnuqqno : buffer time);
end hwumn;

library ieee;
use ieee.std_logic_1164.all;

architecture bnclmaopq of hwumn is
  signal irgzdiwrw : std_logic;
  signal q : integer_vector(3 to 0);
  signal rt : std_logic_vector(2 downto 3);
  signal vlv : std_logic_vector(2 to 3);
  signal fmlvfb : real;
  signal hvlsqtb : time;
begin
  tegzqz : entity work.zhbomyx
    port map (no => hvlsqtb, j => fmlvfb, d => vlv, smqfu => rt);
  m : entity work.lsg
    port map (cohkpiug => q);
  mr : entity work.sijtdzmok
    port map (bnry => irgzdiwrw);
  dnuspsj : entity work.lsg
    port map (cohkpiug => q);
  
  -- Single-driven assignments
  hvlsqtb <= 2#10# ns;
  
  -- Multi-driven assignments
  vlv <= ('Z', '1');
  irgzdiwrw <= 'Z';
  vlv <= "HH";
  rt <= "";
end bnclmaopq;



-- Seed after: 5224650682789351234,17924494779688682807
