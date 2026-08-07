-- Seed: 8357147316595644041,8068158652091157513

entity m is
  port (qeqqmlwgu : out real; bjcvnwjb : out integer; t : in real_vector(1 downto 1));
end m;

architecture f of m is
  
begin
  -- Single-driven assignments
  qeqqmlwgu <= qeqqmlwgu;
  bjcvnwjb <= 2#00101#;
end f;

library ieee;
use ieee.std_logic_1164.all;

entity ejl is
  port (tvctsgof : out std_logic_vector(3 downto 3); gjnbbqr : out bit; t : out real);
end ejl;

architecture btmuicx of ejl is
  signal gqyjibiti : real_vector(1 downto 1);
  signal xbsbrxusa : integer;
  signal embyglgmpe : real_vector(1 downto 1);
  signal hkuuqhizf : integer;
  signal bxuetlthz : real;
  signal gv : real_vector(1 downto 1);
  signal igoy : integer;
  signal kglquth : real;
begin
  koet : entity work.m
    port map (qeqqmlwgu => kglquth, bjcvnwjb => igoy, t => gv);
  f : entity work.m
    port map (qeqqmlwgu => bxuetlthz, bjcvnwjb => hkuuqhizf, t => embyglgmpe);
  hphbxs : entity work.m
    port map (qeqqmlwgu => t, bjcvnwjb => xbsbrxusa, t => gqyjibiti);
  
  -- Single-driven assignments
  gjnbbqr <= '1';
  embyglgmpe <= (others => 16#CF.0#);
  gv <= (others => 8#34.2_6_3#);
  gqyjibiti <= gv;
  
  -- Multi-driven assignments
  tvctsgof <= tvctsgof;
  tvctsgof <= (others => 'X');
  tvctsgof <= (others => 'H');
end btmuicx;



-- Seed after: 16537220359727548208,8068158652091157513
