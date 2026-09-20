-- Seed: 846383201045819265,18037650846010261179

entity n is
  port (zxzexhhpu : in integer_vector(4 downto 2); hldpzwlvno : out bit_vector(1 to 0); vqzoafb : linkage time_vector(2 downto 3));
end n;

architecture ngip of n is
  
begin
  -- Single-driven assignments
  hldpzwlvno <= (others => '0');
end ngip;

entity fvwnj is
  port (dhxo : inout time; iqtthiwo : linkage boolean; cfgevtx : buffer time);
end fvwnj;

architecture vzjm of fvwnj is
  signal hndr : time_vector(2 downto 3);
  signal bygvs : bit_vector(1 to 0);
  signal xiq : integer_vector(4 downto 2);
  signal bfpgxeyp : time_vector(2 downto 3);
  signal xcnh : bit_vector(1 to 0);
  signal mcrbvr : integer_vector(4 downto 2);
begin
  arw : entity work.n
    port map (zxzexhhpu => mcrbvr, hldpzwlvno => xcnh, vqzoafb => bfpgxeyp);
  fbwbn : entity work.n
    port map (zxzexhhpu => xiq, hldpzwlvno => bygvs, vqzoafb => hndr);
  
  -- Single-driven assignments
  cfgevtx <= 3.0 ns;
  xiq <= mcrbvr;
  mcrbvr <= mcrbvr;
  dhxo <= 1 min;
end vzjm;



-- Seed after: 1980571674904545397,18037650846010261179
