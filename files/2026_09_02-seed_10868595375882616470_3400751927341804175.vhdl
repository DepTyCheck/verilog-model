-- Seed: 10868595375882616470,3400751927341804175

entity goan is
  port (ivyb : buffer integer_vector(1 to 3); b : buffer integer_vector(3 downto 4));
end goan;

architecture vr of goan is
  
begin
  -- Single-driven assignments
  b <= (others => 0);
  ivyb <= ivyb;
end vr;



-- Seed after: 1866797393653059995,3400751927341804175
