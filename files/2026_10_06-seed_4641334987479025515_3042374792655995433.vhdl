-- Seed: 4641334987479025515,3042374792655995433

library ieee;
use ieee.std_logic_1164.all;

entity mhkzs is
  port (nhgtylxqc : in string(3 to 1); qewijimnpu : in std_logic_vector(0 to 4); znlvtu : inout std_logic_vector(3 downto 1));
end mhkzs;

architecture mefxnqwjo of mhkzs is
  
begin
  -- Multi-driven assignments
  znlvtu <= ('H', '1', '0');
  znlvtu <= znlvtu;
  znlvtu <= ('1', '0', 'X');
  znlvtu <= ('X', 'U', 'Z');
end mefxnqwjo;

entity rcsev is
  port (ge : linkage bit; i : out real_vector(3 downto 2));
end rcsev;

library ieee;
use ieee.std_logic_1164.all;

architecture yd of rcsev is
  signal ee : string(3 to 1);
  signal wnmopeggh : std_logic_vector(0 to 4);
  signal udn : std_logic_vector(3 downto 1);
  signal eho : std_logic_vector(0 to 4);
  signal kfwjmnoxp : string(3 to 1);
begin
  uikqurexu : entity work.mhkzs
    port map (nhgtylxqc => kfwjmnoxp, qewijimnpu => eho, znlvtu => udn);
  jgaf : entity work.mhkzs
    port map (nhgtylxqc => kfwjmnoxp, qewijimnpu => wnmopeggh, znlvtu => udn);
  udaxv : entity work.mhkzs
    port map (nhgtylxqc => ee, qewijimnpu => eho, znlvtu => udn);
  
  -- Single-driven assignments
  kfwjmnoxp <= "";
  i <= (8#6_1_7.2_4_4_4_2#, 8#5_1_7_1.5_3#);
  
  -- Multi-driven assignments
  eho <= ('Z', 'Z', 'W', 'U', 'H');
  wnmopeggh <= eho;
  eho <= eho;
end yd;

library ieee;
use ieee.std_logic_1164.all;

entity jqjipjvir is
  port (zzxymejhm : out bit; q : out std_logic);
end jqjipjvir;

library ieee;
use ieee.std_logic_1164.all;

architecture baa of jqjipjvir is
  signal zd : std_logic_vector(3 downto 1);
  signal eeuyznm : std_logic_vector(0 to 4);
  signal ww : string(3 to 1);
  signal dueuvd : real_vector(3 downto 2);
  signal wo : bit;
begin
  fnuq : entity work.rcsev
    port map (ge => wo, i => dueuvd);
  ucbqm : entity work.mhkzs
    port map (nhgtylxqc => ww, qewijimnpu => eeuyznm, znlvtu => zd);
  
  -- Single-driven assignments
  ww <= (others => ' ');
  zzxymejhm <= zzxymejhm;
  
  -- Multi-driven assignments
  eeuyznm <= ('L', 'W', 'H', 'H', 'U');
  eeuyznm <= "1ZUU1";
end baa;



-- Seed after: 14135275727558449056,3042374792655995433
