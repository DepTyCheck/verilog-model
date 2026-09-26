-- Seed: 6941886362979517409,10875537289884587119

entity wlmgdz is
  port (diiuas : out boolean_vector(2 downto 3));
end wlmgdz;

architecture ro of wlmgdz is
  
begin
  -- Single-driven assignments
  diiuas <= (others => TRUE);
end ro;

entity voudn is
  port (s : in integer; inap : in integer_vector(3 to 2));
end voudn;

architecture gduwqxj of voudn is
  signal cvgcsdvao : boolean_vector(2 downto 3);
begin
  c : entity work.wlmgdz
    port map (diiuas => cvgcsdvao);
end gduwqxj;



-- Seed after: 5267430708988541042,10875537289884587119
