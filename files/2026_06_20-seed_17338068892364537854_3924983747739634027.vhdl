-- Seed: 17338068892364537854,3924983747739634027

entity kpdtueki is
  port (n : buffer integer; nfee : out severity_level);
end kpdtueki;

architecture jitt of kpdtueki is
  
begin
  -- Single-driven assignments
  nfee <= WARNING;
  n <= 34;
end jitt;



-- Seed after: 1841996101424495016,3924983747739634027
