-- Seed: 4869212169116426787,4292249356257567981

entity pegdgn is
  port (exso : inout bit_vector(0 to 2));
end pegdgn;

architecture taro of pegdgn is
  
begin
  -- Single-driven assignments
  exso <= exso;
end taro;



-- Seed after: 10551403673092693090,4292249356257567981
