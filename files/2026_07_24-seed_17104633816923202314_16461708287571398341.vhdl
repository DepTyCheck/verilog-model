-- Seed: 17104633816923202314,16461708287571398341

library ieee;
use ieee.std_logic_1164.all;

entity iqpmf is
  port (tkgetdnw : in integer; pms : out std_logic_vector(2 downto 3); isyll : in time; imbq : inout integer);
end iqpmf;

architecture g of iqpmf is
  
begin
  -- Single-driven assignments
  imbq <= imbq;
  
  -- Multi-driven assignments
  pms <= "";
  pms <= (others => '0');
  pms <= (others => '0');
  pms <= pms;
end g;

entity dmmqol is
  port (j : buffer severity_level);
end dmmqol;

library ieee;
use ieee.std_logic_1164.all;

architecture lyofwci of dmmqol is
  signal yselhoc : integer;
  signal cfgjifkrq : time;
  signal yvdvshf : std_logic_vector(2 downto 3);
  signal wrr : integer;
begin
  joo : entity work.iqpmf
    port map (tkgetdnw => wrr, pms => yvdvshf, isyll => cfgjifkrq, imbq => yselhoc);
  
  -- Multi-driven assignments
  yvdvshf <= "";
  yvdvshf <= (others => '0');
  yvdvshf <= yvdvshf;
end lyofwci;



-- Seed after: 8863768779332509753,16461708287571398341
