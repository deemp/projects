// SPDX-Licence-Identifier: MIT License

// SPDX-License-Identifier: MIT
pragma solidity 0.8.4;
import "@openzeppelin/contracts/access/Ownable.sol";
import "@openzeppelin/contracts/token/ERC20/ERC20.sol";
import "@openzeppelin/contracts/token/ERC20/extensions/ERC20Burnable.sol";

// owners can destroy their tokens
abstract contract N2DRewards is ERC20, ERC20Burnable, Ownable {
    mapping (address => bool) controllers;

    constructor() ERC20("BRewards", "BRew") {}  

    function mint(address to, uint256 amount) external {
        require(controllers[msg.sender], "Only controllers can mint!");
        _mint(to, amount);
    }

    // account can burn excessive tokens
    function burnFrom(address account, uint256 amount) public override {
        if(controllers[msg.sender]) {
            _burn(account, amount);
        } 
        else {
            super.burnFrom(account, amount);
        }
    }

    function addController(address controller) external onlyOwner {
        controllers[controller] = true;
    }

    function removeController(address controller) external onlyOwner {
        controllers[controller] = false;
    }
}   