-- Seed: 12325589780370909321,15025465285671019065

library ieee;
use ieee.std_logic_1164.all;

entity lmtkmu is
  port (ykljy : buffer std_logic; zaaxvhg : out boolean_vector(1 downto 4); hfvvfsklsl : inout integer);
end lmtkmu;

architecture dg of lmtkmu is
  
begin
  -- Single-driven assignments
  zaaxvhg <= zaaxvhg;
  hfvvfsklsl <= 3;
  
  -- Multi-driven assignments
  ykljy <= 'W';
  ykljy <= ykljy;
  ykljy <= 'W';
  ykljy <= '0';
end dg;

entity cdztsyxeai is
  port (tubwlyl : out real; wxydyjteo : in boolean_vector(0 to 0));
end cdztsyxeai;

library ieee;
use ieee.std_logic_1164.all;

architecture zapgvwop of cdztsyxeai is
  signal tzrdbd : integer;
  signal lscvkswrpz : boolean_vector(1 downto 4);
  signal kdyuql : integer;
  signal cl : boolean_vector(1 downto 4);
  signal ebef : std_logic;
begin
  loredud : entity work.lmtkmu
    port map (ykljy => ebef, zaaxvhg => cl, hfvvfsklsl => kdyuql);
  eqe : entity work.lmtkmu
    port map (ykljy => ebef, zaaxvhg => lscvkswrpz, hfvvfsklsl => tzrdbd);
  
  -- Single-driven assignments
  tubwlyl <= tubwlyl;
  
  -- Multi-driven assignments
  ebef <= ebef;
  ebef <= '-';
end zapgvwop;

entity m is
  port (egedyb : out real_vector(1 downto 4));
end m;

library ieee;
use ieee.std_logic_1164.all;

architecture hcyojkuor of m is
  signal fbalua : integer;
  signal kpjpxlw : boolean_vector(1 downto 4);
  signal thrt : integer;
  signal ovezoyjo : boolean_vector(1 downto 4);
  signal o : std_logic;
  signal duq : boolean_vector(0 to 0);
  signal acrk : real;
begin
  rrvogqcbb : entity work.cdztsyxeai
    port map (tubwlyl => acrk, wxydyjteo => duq);
  hzzrf : entity work.lmtkmu
    port map (ykljy => o, zaaxvhg => ovezoyjo, hfvvfsklsl => thrt);
  pnyooqmlo : entity work.lmtkmu
    port map (ykljy => o, zaaxvhg => kpjpxlw, hfvvfsklsl => fbalua);
  
  -- Single-driven assignments
  egedyb <= (others => 0.0);
  duq <= (others => FALSE);
  
  -- Multi-driven assignments
  o <= '1';
  o <= o;
  o <= o;
  o <= '1';
end hcyojkuor;



-- Seed after: 5594742262271251169,15025465285671019065
