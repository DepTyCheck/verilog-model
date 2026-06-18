-- Seed: 18172031532425603129,8118127366649987907

entity by is
  port (c : out time; faur : buffer bit_vector(3 downto 1); i : buffer bit);
end by;

architecture grcc of by is
  
begin
  -- Single-driven assignments
  i <= '1';
end grcc;



-- Seed after: 9364389687706605481,8118127366649987907
