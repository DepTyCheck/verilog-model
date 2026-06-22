-- Seed: 14957789624920495975,13479070923501788437

library ieee;
use ieee.std_logic_1164.all;

entity vcuoabo is
  port (criorp : buffer real; wzeopscguc : linkage std_logic);
end vcuoabo;

architecture zjnjrjcn of vcuoabo is
  
begin
  -- Single-driven assignments
  criorp <= 3.3;
end zjnjrjcn;

entity irdqcg is
  port (qdc : buffer boolean_vector(2 downto 4); owifajhvee : out string(2 downto 5); aspydvv : buffer real; ns : buffer string(5 to 4));
end irdqcg;

library ieee;
use ieee.std_logic_1164.all;

architecture brq of irdqcg is
  signal jthb : std_logic;
begin
  coxt : entity work.vcuoabo
    port map (criorp => aspydvv, wzeopscguc => jthb);
  
  -- Single-driven assignments
  ns <= (others => ' ');
  qdc <= (others => TRUE);
  owifajhvee <= "";
  
  -- Multi-driven assignments
  jthb <= 'U';
  jthb <= '-';
  jthb <= 'U';
end brq;

entity fdp is
  port (ixhyo : buffer character);
end fdp;

library ieee;
use ieee.std_logic_1164.all;

architecture apivyu of fdp is
  signal dennq : std_logic;
  signal xzegdxn : real;
  signal hsroyyhph : string(5 to 4);
  signal rrxzv : real;
  signal akwkj : string(2 downto 5);
  signal t : boolean_vector(2 downto 4);
begin
  dw : entity work.irdqcg
    port map (qdc => t, owifajhvee => akwkj, aspydvv => rrxzv, ns => hsroyyhph);
  cxaj : entity work.vcuoabo
    port map (criorp => xzegdxn, wzeopscguc => dennq);
  
  -- Single-driven assignments
  ixhyo <= 'y';
  
  -- Multi-driven assignments
  dennq <= 'X';
end apivyu;



-- Seed after: 9215645089458461203,13479070923501788437
