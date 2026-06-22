-- Seed: 4878070400818386621,13479070923501788437

library ieee;
use ieee.std_logic_1164.all;

entity eddpugvmvp is
  port (wasel : buffer std_logic_vector(4 downto 4); gtvdtgv : buffer std_logic; ebrawcsf : inout time_vector(4 to 3); ikbtdcxh : in std_logic);
end eddpugvmvp;

architecture sdxkfqayy of eddpugvmvp is
  
begin
  -- Single-driven assignments
  ebrawcsf <= (others => 0 ns);
  
  -- Multi-driven assignments
  gtvdtgv <= '0';
  gtvdtgv <= 'W';
end sdxkfqayy;

library ieee;
use ieee.std_logic_1164.all;

entity mqsqyq is
  port (rfnge : in integer; sszqjmkzqy : buffer integer; orrpg : inout time; vfgbnzqldf : out std_logic_vector(2 downto 1));
end mqsqyq;

architecture afbxvrys of mqsqyq is
  
begin
  -- Single-driven assignments
  orrpg <= 4000.40 ns;
  sszqjmkzqy <= 3130;
  
  -- Multi-driven assignments
  vfgbnzqldf <= ('H', 'H');
end afbxvrys;

library ieee;
use ieee.std_logic_1164.all;

entity cewq is
  port (v : inout std_logic_vector(2 to 3));
end cewq;

library ieee;
use ieee.std_logic_1164.all;

architecture it of cewq is
  signal gunvut : time_vector(4 to 3);
  signal enni : std_logic;
  signal l : time_vector(4 to 3);
  signal biyk : std_logic;
  signal mthfdh : std_logic_vector(2 downto 1);
  signal igf : time;
  signal puxuupdt : integer;
  signal molvtxq : std_logic;
  signal qoziwuyx : time_vector(4 to 3);
  signal uueuki : std_logic;
  signal eqewrn : std_logic_vector(4 downto 4);
begin
  meb : entity work.eddpugvmvp
    port map (wasel => eqewrn, gtvdtgv => uueuki, ebrawcsf => qoziwuyx, ikbtdcxh => molvtxq);
  tfvl : entity work.mqsqyq
    port map (rfnge => puxuupdt, sszqjmkzqy => puxuupdt, orrpg => igf, vfgbnzqldf => mthfdh);
  kszvefshxs : entity work.eddpugvmvp
    port map (wasel => eqewrn, gtvdtgv => biyk, ebrawcsf => l, ikbtdcxh => enni);
  gfashsjzu : entity work.eddpugvmvp
    port map (wasel => eqewrn, gtvdtgv => molvtxq, ebrawcsf => gunvut, ikbtdcxh => molvtxq);
  
  -- Multi-driven assignments
  eqewrn <= (others => '-');
  uueuki <= 'W';
  v <= ('H', '-');
end it;



-- Seed after: 1044275234002573079,13479070923501788437
